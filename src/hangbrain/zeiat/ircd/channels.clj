(ns hangbrain.zeiat.ircd.user
  "Channel-management commands for the ircd interface."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* numeric reply-from privmsg]]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorState]]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

; TODO numerics should send the registered user's nick instead of * once registration is complete

; TODO: we need to figure out what the datashape of a chat really is
; I suspect something like: :name :ircname :type :id
; but perhaps ircname == id and it's up to the backend to keep track of their
; internal IDs, and the interface uses the ircname exclusively?
(defn- rpl-list
  [{:keys [name count topic]}]
  (numeric 322 name count topic))

(defmethod message :LIST
  [_ & _filter]
  ; TODO: implement filter support
  (numeric 321 "Channel" "Users Name")
  (as-> (.listChannels (:backend *state*)) $
        (run! rpl-list $))
  (numeric 323 "End of LIST"))

(defn- rpl-who
  [{:keys [name user host realname]}]
  (numeric 352 "*" name host "Zeiat" user "H@" (str "0 " realname)))

(defmethod message :WHO
  [_ _filter]
  ; TODO implement filter support -- many clients will send a WHO <channel> as soon
  ; as a channel is successfully joined, so this is actually important.
  (as-> (.listUsers (:backend *state*)) $
        (run! rpl-who $))
  (numeric 315 "End of WHO"))

; How much space does a names line take up? Sum of length of individual names
; + 1 space per name
(defn- names-len
  [names]
  (+ (count names)
     (reduce + (map count names))))

(defn- partition-names
  [names maxwidth]
  (let [[head tail]
        (reduce
          (fn [[head tail] name]
            (let [head' (cons name head)]
              (if (< (names-len head') maxwidth)
                [head' tail]
                [(list name) (cons head tail)])))
          [nil nil] names)]
    (cons head tail)))

(defn- send-names
  [channel names]
  (log/trace "send-names" channel names)
  (as-> names $
        (partition-names $ 4096)
        (run! #(numeric 353 "=" channel (string/join " " %)) $))
  (numeric 366 channel "End of NAMES"))

; Return a valid fully qualified irc name (nick!user@host) from the given struct.
(defn- fqircn
  [state]
  (str (:name state) "!" (:user state) "@zeiat"))

; on join, we need to reply with:
; :nick JOIN #foo
; 332 nick #foo :topic
; 353 nick = #foo :list of user names space separated
; 366 nick #foo :end of names
(defn- join-channel
  "Handle a JOIN of a single channel, given a set of channels we're already joined to, and return the new set.
  Replies with 403 (and makes no changes) if the channel doesn't exist, and silently ignores the message if the user is already on that channel.
  On a successful join, replies with:
  - JOIN
  - 332 (topic)
  - 353/366 (names)"
  [joined channel]
  (let [info (.statChannel (:backend *state*) channel)]
    (cond
      (nil? info)
      (do
        (numeric 403 "No such channel")
        joined)
      (= :dm (:type info))
      (do
        (numeric 403 "Not a channel")
        joined)
      (joined channel) joined ; user is already in this channel, ignore the message
      :else ; joining a channel they aren't on
      (do
        (reply-from (fqircn *state*) "JOIN" channel)
        (numeric 332 channel (:topic info))
        (send-names channel (conj (:names info) (:name *state*)))
        (conj joined channel)))))

(defmethod message :JOIN
  ; TODO: keys might be usable to differentiate otherwise-identical channels
  ; in backends that allow name collisions e.g. Discord
  [_ channels & _keys]
  (let [channels (string/split channels #",")]
    (update *state* :channels #(reduce join-channel % channels))))

(defmethod message :PART
  [_ channels _message]
  *state*)
  ; (let [channels (string/split channels #",")]
  ;   (reduce part-channel *state* channels)))

(defmethod message :NAMES
  [_ channel]
  ; todo factor out common parts with join-channel
  (let [info (.statChannel (:backend *state*) channel)]
    (cond
      (nil? info) (numeric 403 "No such channel")
      (= :dm (:type info)) (numeric 403 "Not a channel")
      :else (send-names channel (:names info))))
  *state*)

(defmethod message :MODE
  ; stubbed out
  [_ channel & _modes]
  (numeric 324 channel "+nt")
  (numeric 329 channel "0")
  *state*)

(defmethod message :PRIVMSG
  [_ channel msg]
  ; TODO: echo the sent message back to the client when it appears, if the client
  ; has negotiated the echo capability
  ; TODO: handle CTCP properly, especially CTCP ACTION
  (if (.writeMessage (:backend *state*) channel msg)
    *state*
    (if (string/starts-with? channel "#")
      (numeric 403 channel "No such channel")
      (numeric 401 channel "No such user"))))

(defmethod message :RECAP
  [_ channel]
  (let [recap (.readMessages (:backend *state*) channel)]
    (log/trace "Done fetching RECAP, total message count:" (count recap))
    (if (nil? recap)
      (numeric 403 channel "No such user/channel")
      (do (run! privmsg recap) *state*))))
