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

(def js-read-messages
  "Collect messages from the currently focused channel. Messages are returned in the order they appear on screen, and look like
  {:author {:realname :user :host} :timestamp :html}
  "
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
  let toMessage = elem => {
    let html =
      elem.querySelector('div[jsaction^=mouseenter] div[jsaction]').innerHTML
      || elem.querySelector('div[jsaction^=mouseenter] a').href
      || 'ERROR: unable to process message';
    if (html == '') {
      html = elem.querySelector()
    }
    return {
     author: toUser(elem.querySelector('span[data-member-id]')),
     timestamp: elem.querySelector('span[data-absolute-timestamp]').dataset.absoluteTimestamp,
     html: html
    };
  };
  return map(toMessage, document.querySelectorAll('c-wiz[data-is-user-topic=true]'));
  ")

(defn process-image
  [img]
  (let [src (second (re-find #"src=\"([^\"]+)\"" img))
        alt (second (re-find #"alt=\"([^\"]+)\"" img))]
    (if alt alt (str "[img " src "]"))))

(defn process-link
  [a]
  (let [href (second (re-find #"href=\"([^\"]+)\"" a))
        text (string/replace a #"</?a[^>]*>" "")]
    (if (or (= text href)
          (= text (string/replace href #"^https?://" "")))
      text
      (str text " [" href "]"))))

(def formatting
  [[#"^<i>(.*)</i>$" "\u0001ACTION$1\u0001"]
   [#"</?b>" "\u0002"]
   [#"</?i>" "\u001D"]
   [#"</?u>" "\u001F"]
   [#"&nbsp;" " "]
   [#"<img [^>]+>" process-image]
   [#"<a [^>]+>.*?</a>" process-link]
   [#"</?span[^>]*>" ""]])

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

(defn- ->IRCMessage
  [message]
  (assoc message
    :author (assoc (:author message) :name (->IRCNick (get-in message [:author :realname])))
    :text (reduce
            ; (fn [text [pattern replacement]] (string/replace text pattern replacement))
            (partial apply string/replace)
            (:html message)
            formatting)))

(defn- read-messages
  [ctx]
  ; TODO factor this out, maybe make find-input-div itself block
  (wd/wait-predicate
    (partial find-input-div ctx)
    {:timeout 5 :message "Couldn't find input div after 5 seconds"})
  (let [iframe (first (find-input-div ctx))]
    (with-frame-el ctx iframe
      (->> (wd/js-execute ctx js-read-messages)
           (map ->IRCMessage)))))

(defn- read-messages-since
  [ctx timestamp]
  (->> (read-messages ctx)
       (drop-while #(<= timestamp (:timestamp %)))))

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

(def js-list-room-channels
  "
  let map = (f,xs) => Array.prototype.map.call(xs, f);
  let toChat = span => {
    return {
      id: span.dataset.groupId,
      timestamp: span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp || 0,
      topic: span.querySelector('span[role=presentation][title]').title,
      unread: span.innerText.includes('Unread'),
      users: [],
      count: 0,
      type: 'channel'
    };
  };
  return map(toChat, document.querySelectorAll('span[role=listitem]'));
  ")

(def js-list-user-channels
  "JS code to read the chat list in the top navigation iframe. Conveniently, the chat entries in this iframe have information about the participants embedded in them.
  This contains three different kinds of chats:
  - DMs, which have one user present and should be represented as users;
  - ad hoc channels, which have multiple users and a name derived from those of all the participants;
  - and defunct channels, which have zero users and should be elided entirely.
  "
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
  let toChat = span => {
    let id = span.dataset.groupId;
    let users = map(toUser, span.querySelectorAll('span[data-member-id]'));
    let timestamp = span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp || 0;
    let unread = span.innerText.includes('Unread');
    if (users.length > 1) {
      let topic = span.querySelector('span[role=presentation][title]').title;
      return { id: id, users: users, topic: topic, timestamp: timestamp, unread: unread, count: users.length, type: 'channel' };
    } else if (users.length == 1) {
      return Object.assign(users[0], { id: id, timestamp: timestamp, unread: unread, type: 'dm' });
    } else {
      return false;
    }
  };
  return map(toChat, document.querySelectorAll('span[role=listitem]')).filter(x=>x);
  ")

(defn ->ChatInfo [info]
  (log/trace "->ChatInfo" info)
  (let [info (update info :type keyword)
        seen (if (:unread info) 0 (:timestamp info))
        name (case (:type info)
               :dm (->IRCNick (:realname info))
               :channel (->IRCChannel (:topic info)))]
    (-> info
        (assoc :seen seen :name name)
        (dissoc :unread))))

(defn list-dms [ctx iframe]
  (log/trace "Getting chats in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/js-execute ctx js-list-user-channels)
         (map ->ChatInfo)
         )))

(defn list-rooms [ctx iframe]
  (log/trace "Getting rooms in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/js-execute ctx js-list-room-channels)
         (map ->ChatInfo)
         )))

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

(defn update-or-insert
  "Update a cache entry or insert a new one.
  The :seen field from the old entry, if present, is retained, so we don't forget about read status every time we refresh the cache."
  [chat chat']
  (merge chat' (select-keys chat [:seen])))

(defn rebuild-cache
  "Rebuild the channel cache.
  This rescans the chat list completely and populates the cache from it, retaining the :seen field from any previous cache entries."
  ; TODO: we should clear cache entries that no longer exist on the server!
  ; this is probably easier if list-all returns a map rather than a list, and then we can do cache -> (select-keys cache (keys new-channels))
  [cache ctx]
  (reduce
    (fn [cache chat]
      (log/trace "Cache update:" (:name chat) "->" (:id chat))
      (update cache (:name chat) update-or-insert chat))
    cache
    (list-all ctx)))

(defn- read-cache
  ([cache ctx] ; read all cache entries -> always rebuild
   (vals (swap! cache rebuild-cache ctx)))
  ([cache ctx key] ; read specific entry -> rebuild on cache miss
   (if (contains? @cache key)
     (@cache key)
     (do
       (log/warn "Cache miss on key" key "- rebuilding")
       (get (swap! cache rebuild-cache ctx) key)))))

(defn clear-unread
  [cache channel]
  (swap! cache
         (fn [cache]
           (assoc-in cache [channel :seen]
             (get-in cache [channel :timestamp])))))

(defn- post-process
  [chat me messages]
  (if (= :dm (:type chat))
    (map
      (fn [message]
        (log/trace "post-process" me message)
        (if (= me (:author message))
          (assoc message :from :me :to chat)
          (assoc message :from chat :to :me)))
      messages)
    (map
      (fn [message]
        (log/trace "post-process" me message)
        (if (= me (:author message))
          (assoc message :from :me :to chat)
          (assoc message :from (:author message) :to chat)))
      messages)))

(defn get-logged-in-account
  [ctx]
  (let [info
        (wd/js-execute
          ctx "return document.querySelector('header[role=banner] a[aria-label*=Account]').getAttribute('aria-label')")
        [_ realname user host] (re-find #"Google Account: ([^\n]+)\n\(([^@]+)@(.*)\)" info)]
    {:name (->IRCNick realname)
     :realname (string/trim realname)
     :user user
     :host host}))

(defn create :- (s/protocol ZeiatBackend)
  [opts]
  (let [ctx (atom nil) ; context for the webdriver
        ; cache mapping IRC channels/nicks to chat information maps
        ; the cache is updated:
        ; - whenever a cache miss occurs
        ; - whenever a request for the entire cache (listUsers/Channels) occurs
        ;   and the cache contents are more than an hour old
        cache (atom {})
        ; A name/user/host/realname struct containing information about the user
        ; we're logged in as
        me (atom nil)]
    (reify ZeiatBackend
      (connect [this]
        (log/info "Connecting to Google Chat...")
        (swap! ctx startup-browser (:browser opts) (:profile opts))
        (reset! me (get-logged-in-account @ctx))
        (log/info "Connected."))
      (disconnect [this]
        (if @ctx
          (do
            (log/info "Shutting down Google Chat connection...")
            (wd/quit @ctx)
            (reset! ctx nil)
            (log/info "Shut down."))
          (log/info "Ignoring shutdown request, backend already shut down.")))
      (listChannels [this]
        (filter #(= :channel (:type %))
          (read-cache cache @ctx)))
      (listUsers [this]
        (filter #(= :dm (:type %))
          (read-cache cache @ctx)))
      (listUnread [this]
        (filter #(not= (:seen %1) (:timestamp %1))
          (read-cache cache @ctx)))
      (listMembers [this channel]
        ; TODO: this works only for channels which have a list of users in the cache entry,
        ; which does not include named channels; to get the user list for those, it's kind of
        ; grody. We need to select the channel, then click on the "channel name [n members]"
        ; button in the top left, then "view members", then find the iframe that filled in and
        ; read the list out of that...this is gross enough that it may be easier to just scan
        ; the channel history, build a user list based on that, and if it changes re-issue a
        ; NAMES command.
        ; This does require some way to tell Zeiat that something changed...eurgh. We're going
        ; to need to extend the API to allow the backend to send events back to Zeiat in some
        ; capacity, I think.
        (when-let [chat (read-cache cache @ctx channel)]
          (:users chat)))
      (readMessages [this channel]
        ; TODO this needs some additional design work
        ; right now a returned message has a timestamp, author, and text
        ; I think we additionally need :to and :from, which are IRC nicks
        ; in the case of DMs, every message is either :to or :from the DM target,
        ; with the other field being the logged in user
        ; in the case of channels, every message is :to the channel and :from the author
        ; in both cases, if one of these fields is the logged in user, it should be
        ; remapped to the IRC user so that they see the messages as coming from themself
        ; rather than from whatever name they use in googlechat
        ; this is critical for DMs (as otherwise the client may just discard them as malformed)
        ; and good UX for channels
        ; it is also possible that we might extend the (connect) protocol to allow the connector
        ; to enforce a nick on the user, in which case the server can change their nick as
        ; soon as they connect to match the logged in nick...something to think about.
        (log/trace "Reading messages from" channel)
        (when-let [chat (read-cache cache @ctx channel)]
          (select-chat @ctx (:id chat))
          (clear-unread cache channel)
          (post-process chat @me (read-messages @ctx))))
      (readNewMessages [this channel]
        (when-let [chat (read-cache cache @ctx channel)]
          (select-chat @ctx (:id chat))
          (let [seen (:seen chat)]
            (clear-unread cache channel)
            (post-process chat @me (read-messages-since @ctx seen)))))
      (writeMessage [this channel message]
        ; TODO handle translation from IRC formatting codes to Hangouts markdownish codes
        ; https://support.google.com/chat/answer/7649118?hl=en
        (log/trace "writeMessage" channel message)
        (when-let [chat (read-cache cache @ctx channel)]
          (log/trace "context from cache:" chat)
          (select-chat @ctx (:id chat))
          (send-message @ctx message)
          true)))))
