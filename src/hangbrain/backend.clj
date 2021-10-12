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
    [clojure.string :as string]
    [zeiat.backend :refer [ZeiatBackend]])
  (:import
    [org.apache.commons.text StringEscapeUtils]))

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
    // TODO this is pretty gross when dealing with links/embeds -- do something better
    let html =
      elem.querySelector('div[jsaction^=mouseenter] div[jsaction]').innerHTML
      || elem.querySelector('div[jsaction^=mouseenter] a').href
      || 'ERROR: unable to process message';
    if (html == '') {
      html = elem.querySelector()
    }
    return {
     author: toUser(elem.querySelector('span[data-member-id]')),
     timestamp: elem.querySelector('span[data-absolute-timestamp]').dataset.absoluteTimestamp.split('.')[0],
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
    (cond
      (or (= text href)
          (= text (string/replace href #"^https?://" "")))
      text
      (not (empty? (string/trim text))) (str "<" href "> " text)
      :else (str "<" href "> "))))

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
   [#"&[^;]+;" #(StringEscapeUtils/unescapeHtml4 %)]
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
            gchat->irc)))

(defn- read-messages
  [ctx]
  ; TODO factor this out, maybe make find-input-div itself block
  (wd/wait-predicate
    (partial find-input-div ctx)
    {:timeout 10 :message "Couldn't find input div after 10 seconds"})
  (let [iframe (first (find-input-div ctx))]
    (with-frame-el ctx iframe
      (->> (wd/js-execute ctx js-read-messages)
           (map ->IRCMessage)))))

(defn- read-messages-since
  [ctx timestamp]
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
    (with-frame-el ctx frame
      (wd/fill-el ctx div (irc->gchat msg) keys/enter))))

(def js-list-room-channels
  "
  let map = (f,xs) => Array.prototype.map.call(xs, f);
  let toChat = span => {
    return {
      id: span.dataset.groupId,
      timestamp: span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp.split('.')[0] || '0',
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
    let timestamp = span.querySelector('span[data-absolute-timestamp]')?.dataset.absoluteTimestamp.split('.')[0] || '0';
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
  ; if we're generating the info for a channel this doesn't set up :users right
  (let [info (update info :type keyword)
        seen (if (:unread info) "0" (:timestamp info))
        name (case (:type info)
               :dm (->IRCNick (:realname info))
               :channel (->IRCChannel (:topic info)))]
    (-> info
        (assoc :seen seen :name name)
        (update :users
          (fn [users] (->> users
                           (map #(assoc % :type :dm))
                           (map ->ChatInfo))))
        )))

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
  (doto (wd/chrome-headless {:args [(str "--user-data-dir=" profile)]
     :path-browser bin
     :locator "css selector"})
    (wd/go "https://chat.google.com/")
    (wd/wait-exists "div#talk_roster" {:timeout 30 :interval 1})))

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

(defn- log-also
  [msg v]
  (log/trace msg v)
  v)

(defn create :- (s/protocol ZeiatBackend)
  ; TODO: we shouldn't permit multiple connections, like, at all.
  [opts]
  (let [ctx (atom nil) ; context for the webdriver
        ; A name/user/host/realname struct containing information about the user
        ; we're logged in as
        me (atom nil)]
    (reify ZeiatBackend
      (connect [this _user]
        (log/info "Connecting to Google Chat...")
        (swap! ctx startup-browser (:browser opts) (:profile opts))
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
          (list-all @ctx)))
      (listUsers [this]
        (filter #(= :dm (:type %))
          (list-all @ctx)))
      (listChatStatus [this]
        (->> (list-all @ctx)
             (map (fn [{:keys [type name timestamp unread] :as chat}]
                     {:status (if unread :unread :read)
                      :last-seen timestamp
                      :name name
                      :type type}))))
      (statChannel [this channel]
        (->> (list-all @ctx)
             (filter #(= channel (:name %)))
             first
             (log-also "stat-channel")))
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
        (when-let [chat (.statChannel this channel)]
          (select-chat @ctx (:id chat))
          (post-process chat @me (read-messages @ctx))))
      (readMessagesSince [this channel id]
        (when-let [chat (.statChannel this channel)]
          (select-chat @ctx (:id chat))
          (post-process chat @me (read-messages-since @ctx id))))
      (writeMessage [this channel message]
        ; TODO handle translation from IRC formatting codes to Hangouts markdownish codes
        ; https://support.google.com/chat/answer/7649118?hl=en
        (log/trace "writeMessage" channel message)
        (when-let [chat (.statChannel this channel)]
          (log/trace "context from statChannel:" chat)
          (select-chat @ctx (:id chat))
          (send-message @ctx message)
          true)))))
