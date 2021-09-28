(ns hangbrain.backend
  "Interface to the headless browser that runs Googlechat."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [etaoin.api :as wd]
    [io.aviso.repl]
    [io.aviso.logging]
    [etaoin.keys :as keys]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.tools.cli :as cli]
    [clojure.string :as string]
    [hangbrain.zeiat :as zeiat]
    ; [hangbrain.zeiat.types :refer [ZeiatBackend]]
    [hangbrain.zeiat.backend :refer [Channel ZeiatBackend]]
    ))

(defn id->selector
  [id]
  (str "#" (string/replace id "/" "\\/")))

(defn el->id
  [ctx el]
  (wd/get-element-attr-el ctx el :id))

(defn maybe-get-attr
  [ctx root tag attr]
  (try
    (wd/get-element-attr ctx [root (str (name tag) "[" (name attr) "]")] attr)
    (catch Exception _ nil)))

(defn ad-hoc-channel-name
  [ctx selector]
  ; we should save the id here (or rather, the demangled form of it) as the ID
  ; for later context switching
  ; then rather than needing to find the button we just set window.location.hash = "#chat/<id>"
  (wd/get-element-attr ctx [selector "span[role=presentation][title]"] :title))

(defmacro with-frame-el
  [ctx frame & body]
  `(try
     (wd/switch-frame* ~ctx (wd/el->ref ~frame))
     ~@body
     (finally (wd/switch-frame-parent ~ctx))))

(defmacro with-frame-n
  [ctx frame & body]
  `(try
     (wd/switch-frame* ~ctx ~frame)
     ~@body
     (finally (wd/switch-frame-parent ~ctx))))

(defn select-chat
  [ctx id]
  (log/trace "selecting chat" id)
  (let [hash (str "chat/" id)]
    (when (not= hash (wd/get-hash ctx))
      (wd/set-hash ctx (str "chat/" id))
      )))

(defn find-input-div
  [ctx]
  (as-> (wd/query-all ctx "iframe[title=\"Chat content\"]") $
        (map (fn [frame] (with-frame-el ctx frame
                        [frame (wd/query-all ctx "div[spellcheck]")])) $)
        (some (fn [[frame divs]] (when (not-empty divs) [frame (first divs)])) $)))

(defn send-message
  [ctx msg]
  (log/trace "sending message" msg)
  (wd/wait-predicate
    (partial find-input-div ctx)
    {:timeout 5 :message "Couldn't find input div after 5 seconds"})
  (let [[frame div] (find-input-div ctx)]
    (log/trace "sending to" frame div)
    (with-frame-el ctx frame
      (wd/fill-el ctx div msg keys/enter))))

(defn ->IRCNick
  [name]
  (-> name
      string/trim
      (string/replace #"[ !@:]+", "-")))

(defn ->IRCChannel
  [name]
  (as-> name $
      (string/trim $)
      (string/replace $ #", +", "+")
      (string/replace $ #"[ ,]+", "-")
      (str "#" $)))

(defn el->RoomChannel
  [ctx el]
  (let [selector (id->selector (el->id ctx el))
        ; timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        ; _ (log/trace "got timestamp" timestamp)
        ]
    {:id (wd/get-element-attr ctx selector :data-group-id)
     :name (->IRCChannel (ad-hoc-channel-name ctx selector))
     :topic (ad-hoc-channel-name ctx selector)
     :count 0  ; FIXME
     :users []
     ; :timestamp timestamp
     :type :channel}))

(def js-list-user-channels
  "
  let map = (f,xs) => Array.prototype.map.call(xs, f);
  let toUser = span => {
    let name = span.dataset.name;
    let [user,host] = span.dataset.hovercardId.split('@');
    return {
      realname: name,
      user: user,
      host: host,
    };
  };
  let chats = [];
  let toChat = span => {
    let id = span.dataset.groupId;
    let users = map(toUser, span.querySelectorAll('span[data-member-id]'));
    let timestamp = span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp || 0;
    if (users.length > 1) {
      let topic = span.querySelector('span[role=presentation][title]').title;
      return { id: id, users: users, topic: topic, timestamp: timestamp, type: 'channel' };
    } else if (users.length == 1) {
      return Object.assign(users[0], { id: id, timestamp: timestamp, type: 'dm' });
    } else {
      return false;
    }
  };
  return map(toChat, document.querySelectorAll('span[role=listitem]')).filter(x=>x);
  ")

(defn ->ChatInfo [info]
  (log/trace "->ChatInfo" info)
  (let [info (update info :type keyword)]
    (case (:type info)
      :dm (assoc info :name (->IRCNick (:realname info)))
      :channel (assoc info
                 :count (count (:users info))
                 :name (->IRCChannel (:topic info)))
    )))

(defn list-dms [ctx iframe]
  (log/trace "Getting chats in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/js-execute ctx js-list-user-channels)
         (map ->ChatInfo)
         )))
         ; (into {})))) ; for later, right now it expects a seq

(defn list-rooms [ctx iframe]
  (log/trace "Getting rooms in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->RoomChannel ctx))
         doall)))

(defn list-all [ctx]
  (let [[dm-iframe channel-iframe] (wd/query-all ctx "div[role=navigation] iframe")]
    (concat
      (list-rooms ctx channel-iframe)
      (list-dms ctx dm-iframe)
      )))

(defn- startup-browser [ctx bin profile]
  (assert (nil? ctx) "Attempt to startup browser when it's already running!")
  (doto (wd/chrome {:args [(str "--user-data-dir=" profile)]
     :path-browser bin
     :locator "css selector"})
    (wd/go "https://chat.google.com/")
    (wd/wait-exists "div#talk_roster" {:timeout 30 :interval 1})))

(defn- timestamp [] (quot (System/currentTimeMillis) 1000))

(defn- rebuild-cache [ctx]
  (reduce
    (fn [cache chat]
      (log/trace "Cache update:" (:name chat) "->" (:id chat))
      (assoc cache (:name chat) chat))
    {:ts (timestamp)}
    (list-all ctx)))

(defn rebuild-if-old
  [cache ctx]
  (if (> (- (timestamp) (:ts @cache)) (* 60 60))
    (do
      (log/info "Cache ts" (:ts @cache) "older than now of" (timestamp) "- rebuilding")
      (reset! cache (rebuild-cache ctx)))
    @cache))

(defn- read-cache
  ([cache ctx]
   (vals (rebuild-if-old cache ctx)))
  ([cache ctx key]
   (if (contains? @cache key)
     (@cache key)
     (do
       (log/warn "Cache miss on key" key "- rebuilding")
       (get (reset! cache (rebuild-cache ctx)) key)))))

(defn create :- (s/protocol ZeiatBackend)
  [opts]
  (let [ctx (atom nil) ; context for the webdriver
        ; cache mapping IRC channels/nicks to chat information maps
        ; the cache is updated:
        ; - whenever a cache miss occurs
        ; - whenever a request for the entire cache (listUsers/Channels) occurs
        ;   and the cache contents are more than an hour old
        cache (atom {:ts 0})]
    (reify ZeiatBackend
      (connect [this]
        (log/info "Connecting to Google Chat...")
        (swap! ctx startup-browser (:browser opts) (:profile opts)))
      (disconnect [this]
        (if @ctx
          (do
            (log/info "Shutting down Google Chat connection...")
            (wd/quit @ctx)
            (reset! ctx nil))
          (log/info "Ignoring shutdown request, backend already shut down.")))
      (listChannels [this]
        (filter #(= :channel (:type %))
          (read-cache cache @ctx)))
      (listUsers [this]
        (filter #(= :dm (:type %))
          (read-cache cache @ctx)))
      (listUnread [this]
        (log/trace "stub: list-unread" this)
        [])
      (listMembers [this channel]
        (log/trace "stub: list-members" this channel)
        [])
      (readMessages [this channel]
        (log/trace "stub: read-messages" this channel)
        [])
      (writeMessage [this channel message]
        ; TODO handle translation from IRC formatting codes to Hangouts markdownish codes
        ; https://support.google.com/chat/answer/7649118?hl=en
        (log/trace "writeMessage" channel message)
        (when-let [chat (read-cache cache @ctx channel)]
          (log/trace "context from cache:" chat)
          (select-chat @ctx (:id chat))
          (send-message @ctx message)
          true)))))
