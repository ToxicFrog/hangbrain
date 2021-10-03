(ns hangbrain.zeiat.ircd.channels
  "Channel-management commands for the ircd interface."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* numeric reply-from]]
    [hangbrain.zeiat.backend :as backend]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

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
  (let [info (backend/stat-channel (:backend *state*) channel)]
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
  (let [info (backend/stat-channel (:backend *state*) channel)]
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
