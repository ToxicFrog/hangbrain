(ns hangbrain.backend
  "Interface to the headless browser that runs Googlechat."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [etaoin.api :as wd]
    [etaoin.keys :as keys]
    [hangbrain.channels :as channels]
    [hangbrain.util :as util]
    [io.aviso.repl]
    [io.aviso.logging]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [taoensso.timbre :as log]
    [zeiat.backend :refer [ZeiatBackend]])
  (:import
    [org.apache.commons.text StringEscapeUtils]))

(defn select-chat
  [ctx id]
  (log/trace "selecting chat" id)
  (let [hash (str "chat/" id)]
    (when (not= hash (wd/get-hash ctx))
      (wd/set-hash ctx (str "chat/" id)))))

(defn find-input-div
  [ctx]
  (as-> (wd/query-all ctx "iframe[title=\"Chat content\"]") $
        (map (fn [frame] (util/with-frame-el ctx frame
                        [frame (wd/query-all ctx "div[spellcheck]")])) $)
        (some (fn [[frame divs]] (when (not-empty divs) [frame (first divs)])) $)))

(defn process-image
  [img]
  (let [src (second (re-find #"src=\"([^\"]+)\"" img))
        alt (second (re-find #"alt=\"([^\"]+)\"" img))]
    (if alt alt (str "[img " src "]"))))

(defn process-link
  [a]
  (let [attachment (re-find #"https://chat.google.com/api/get_hangouts_attachment_url\?[^\"]*url_type=[^\"]+" a)
        href (second (re-find #"href=\"([^\"]+)\"" a))]
    ; we don't care about the text because gchat doesn't let you send a link with text that differs
    ; from the link target
    (or attachment href)))

(def gchat->irc
  "Formatting map for turning gchat messages into IRC ones. Mostly this means stripping/translating HTML tags."
  ; TODO: Chat lacks proper CTCP ACTION support, so instead it just slaps the
  ; user's firstname on the front of the message and wraps it in italics. This
  ; means that if we just unwrap it it still has their first name prefixed to
  ; the message payload and shows up as, e.g,
  ;  * Zhu-Li Zhu does the thing.
  ; rather than as
  ;  * Zhu-Li does the thing
  ; or as
  ;  * Zhu does the thing
  ; for now we handle this by dropping the first word of the message, which is
  ; correct-ish and at least means that their name in the /me will still match
  ; up with their name in other messages; that said, we might want to consider
  ; adding an option that lets it just use firstname as display name, or something;
  ; This is also an issue for outgoing CTCPs, which we don't support at all yet.
  [[#"^<i>[^ ]+ (.*)</i>$" "\u0001ACTION $1\u0001"]
   [#"</?b>" "\u0002"]
   [#"</?i>" "\u001D"]
   [#"</?u>" "\u001F"]
   [#"<a [^>]+>.*?</a>" process-link]
   [#"<img [^>]+>" process-image]
   [#"&[^;]+;" #(StringEscapeUtils/unescapeHtml4 %)]
   [#"</?span[^>]*>" ""]])

(defn- ->IRCMessage
  [message]
  (assoc message
    :author (assoc (:author message) :name (util/->IRCNick (get-in message [:author :realname])))
    :timestamp (util/millis->datetime (:timestamp message))
    :text (reduce
            ; (fn [text [pattern replacement]] (string/replace text pattern replacement))
            (partial apply string/replace)
            (:html message)
            gchat->irc)))

(def js-read-messages
  (-> "read-messages.js" io/resource slurp))

; TODO sometimes we get a message with "\n" in it back from firefox rather than
; multiple lines of messages or a message with a real newline?????
; search logs for "discovered this existed" for an example
(defn- read-messages
  [ctx]
  ; TODO factor this out, maybe make find-input-div itself block
  (wd/wait-predicate
    (partial find-input-div ctx)
    {:timeout 10 :message "Couldn't find input div after 10 seconds"})
  (let [iframe (first (find-input-div ctx))]
    (util/with-frame-el ctx iframe
      (wd/scroll-bottom ctx)
      (->> (wd/js-execute ctx js-read-messages)
           (map ->IRCMessage)))))

(defn- read-messages-since
  [ctx timestamp]
  ; TODO if the timestamp is not found we should return everything, not nothing
  (log/trace "timestamp" timestamp (type timestamp))
  (->> (read-messages ctx)
       (drop-while #(<= (compare (:timestamp %) timestamp) 0))))


(def irc-to-gchat-formatting
  "Formatting map for translating IRC formatting codes to gchat ones."
  [["\u0002" "*"] ; bold
   ["\u001D" "_"] ; italics
   ["\u001F" ""] ; underline -- not supported :(
   ])

(defn- irc->gchat
  [msg]
  (reduce (partial apply string/replace) msg irc-to-gchat-formatting))

(defn send-message
  [ctx msg]
  (log/trace "sending message" msg)
  (wd/wait-predicate
    (partial find-input-div ctx)
    {:timeout 10 :message "Couldn't find input div after 10 seconds"})
  (let [[frame div] (find-input-div ctx)]
    (log/trace "sending to" frame div)
    (util/with-frame-el ctx frame
      (wd/fill-el ctx div (irc->gchat msg) keys/enter))))

(defn- create-webdriver-context [{:keys [browser listen-port debug]}]
  ; TODO we need to write the marionette.port preference to user.js in the profile directory
  ; user_pref("marionette.port", XXXX)
  ; how do we find the profile directory?
  ; we can't rely on the dirs library, because it's in ~/.mozilla/firefox on linux,
  ; which is not one of the standard locations.
  (doto ((if debug wd/firefox wd/firefox-headless)
         {:args ["-P" "ca.ancilla.hangbrain"]
          :args-driver ["--marionette-port" (dec listen-port)] ; "--log" "trace"
          ; :log-stdout "/dev/tty"
          ; :log-stderr "/dev/tty"
          :size [1920 2160]
          :url "https://chat.google.com/"
          :path-browser browser
          :locator "css selector"})
    (wd/wait-exists "div#talk_roster" {:timeout 30 :interval 1})
    ; We need to wait a bit, because all the chats load as "unread" and if we
    ; start fetching chat status immediately, all the chats will appear as unread
    ; and we will have a bad time.
    (wd/wait 5)))

(defn- startup-browser [ctx {:keys [listen-port] :as options}]
  (assert (nil? ctx) "Attempt to startup browser when it's already running!")
  (log/info "Starting browser with marionette port" (dec listen-port) "and profile ca.ancilla.hangbrain")
  (create-webdriver-context options))

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
    {:name (util/->IRCNick realname)
     :realname (string/trim realname)
     :user user
     :host host}))

(defn- log-also
  [msg v]
  (log/trace msg v)
  v)

(defn create :- (s/protocol ZeiatBackend)
  [opts _reply_fn]
  (let [ctx (atom nil) ; context for the webdriver
        ; A name/user/host/realname struct containing information about the user
        ; we're logged in as
        me (atom nil)]
    (reify ZeiatBackend
      (connect [this _user]
        (log/info "Connecting to Google Chat...")
        (swap! ctx startup-browser opts)
        (reset! me (get-logged-in-account @ctx))
        (log/info "Connected.")
        (str "Connected to chat.google.com as '"
          (:realname @me)
          "' (" (:user @me) "@" (:host @me) ")"))
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
          (channels/list-all @ctx)))
      (listUsers [this]
        (filter #(= :dm (:type %))
          (channels/list-all @ctx)))
      (listChatStatus [this]
        (->> (channels/list-all @ctx)
             (map (fn [{:keys [type name timestamp unread] :as chat}]
                    ; (log/trace "listChatStatus" chat)
                     {:status (if unread :unread :read)
                      :last-seen timestamp
                      :name name
                      :type type}))))
      (statChannel [this channel]
        (->> (channels/list-all @ctx)
             (filter #(= channel (:name %)))
             first
             (log-also "stat-channel")))
      (readMessages [this channel]
        (log/trace "Reading messages from" channel)
        (when-let [chat (.statChannel this channel)]
          (select-chat @ctx (:id chat))
          (post-process chat @me (read-messages @ctx))))
      (readMessagesSince [this channel id]
        (when-let [chat (.statChannel this channel)]
          (select-chat @ctx (:id chat))
          (post-process chat @me (read-messages-since @ctx id))))
      (writeAction [this channel action]
        (.writeMessage this channel (str "*" action "*")))
      (writeMessage [this channel message]
        (log/trace "writeMessage" channel message)
        (when-let [chat (.statChannel this channel)]
          (log/trace "context from statChannel:" chat)
          (select-chat @ctx (:id chat))
          (send-message @ctx message)
          true)))))
