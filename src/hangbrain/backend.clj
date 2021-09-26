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

(defn el->selector
  [ctx el]
  (log/trace "getting id for element" el)
  (str
    "#"
    (string/replace
       (wd/get-element-attr-el ctx el :id)
       "/" "\\/")))

(defn maybe-get-attr
  [ctx root tag attr]
  (try
    (wd/get-element-attr ctx [root (str (name tag) "[" (name attr) "]")] attr)
    (catch Exception _ nil)))

(defn ad-hoc-channel-name
  [ctx id]
  (log/trace "ad hoc channel name" id)
  (wd/get-element-attr ctx [id "span[role=presentation][title]"] :title))

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

(defn select-channel
  [ctx [iframe el]]
  (with-frame-el ctx iframe
    (wd/click-el ctx el)))

(defn send-message
  [ctx msg]
  (with-frame-n ctx 5
   (wd/fill ctx "div[spellcheck]" msg)
   (wd/fill ctx "div[spellcheck]" keys/enter)
  ))

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

(defn dm-info
  [ctx id]
  (let [name (wd/get-element-attr ctx [id "span[data-member-id]"] :data-name)
        email (wd/get-element-attr ctx [id "span[data-member-id]"] :data-hovercard-id)
        [_ user host] (re-matches #"([^@]+)@(.+)" email)]
  {:name (->IRCNick name)
   :user user
   :realname name
   :host host
   :type :dm}))

; if we innerText a DM, the first line is going to be "Active", "Away", etc
; if there are unread messages, the second line will be "Unread"
; for DMs, inside the el, we're going to have a <span> with
;  data-hovercard-id=<email>
;  data-name=<human-readable name>
; timestamp is in span[data-absolute-timestamp]
; for ad hoc rooms there are going to be *multiple* spans with those elements,
; one per user in the room
; for channels, there's going to be a <span> with role=presentation title=<channel name>
(defn el->UserChannel
  [ctx iframe el]
  (let [id (el->selector ctx el)
        _ (log/trace "UserChannel" id)
        users (wd/query-all ctx [id "span[data-member-id]"])
        _ (log/trace "got user list")
        ; timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        _ (log/trace "got timestamp")]
    (cond
      (empty? users) nil
      (= 1 (count users)) (dm-info ctx id)
      :else ; ad hoc channel
      {:id [iframe el]
       :name (->IRCChannel (ad-hoc-channel-name ctx id))
       :topic (ad-hoc-channel-name ctx id)
       :count (count users)
       :users [] ; FIXME
       ; :timestamp timestamp
       :type :channel}
      )))

(defn el->RoomChannel
  [ctx iframe el]
  (let [id (el->selector ctx el)
        timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        _ (log/trace "got timestamp" timestamp)]
    {:id [iframe el]
     :name (->IRCChannel (ad-hoc-channel-name ctx id))
     :topic (ad-hoc-channel-name ctx id)
     :count 0  ; FIXME
     :users []
     ; :timestamp timestamp
     :type :channel}))

(defn list-dms [ctx iframe]
  (log/trace "Getting chats in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->UserChannel ctx iframe))
         doall)))

(defn list-rooms [ctx iframe]
  (log/trace "Getting rooms in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->RoomChannel ctx iframe))
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

; (defn- cache-key
;   [chat]
;   (case (:type chat)
;     :channel (:name chat)))

; (defn- update-cache
;   [cache ctx]
;   (->> (list-all ctx)
;        (reduce
;          (fn [cache chat] (assoc cache ())))
;   )

(defn create :- (s/protocol ZeiatBackend)
  [opts]
  (let [ctx (atom nil) cache (atom nil)]
    (reify ZeiatBackend
      (connect [this]
        (log/info "Connecting to Google Chat...")
        (swap! ctx startup-browser (:browser opts) (:profile opts)))
      (disconnect [this]
        (log/info "Shutting down Google Chat connection...")
        (swap! ctx wd/quit))
      (listChannels [this]
        (->> (list-all @ctx)
             (filter #(= :channel (:type %)))))
      (listUsers [this]
        (->> (list-all @ctx)
             (filter #(= :dm (:type %)))))
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

        (log/trace "stub: write-message" this channel message)))))
