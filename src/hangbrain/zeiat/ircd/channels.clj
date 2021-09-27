(ns hangbrain.zeiat.ircd.user
  "Channel-management commands for the ircd interface."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* numeric reply-from]]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorState]]
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
  ; TODO implement filter support
  (as-> (.listUsers (:backend *state*)) $
        (run! rpl-who $))
  (numeric 315 "End of WHO"))

; on join, we need to reply with:
; :nick JOIN #foo
; 332 nick #foo :topic
; 353 nick = #foo :list of user names space separated
; 366 nick #foo :end of names
(defmethod message :JOIN
  [_ channels]
  *state*)
  ; (let [channels (string/split channels #",")]
  ;   (reduce join-channel *state* channels)))

(defmethod message :PART
  [_ channels _message]
  *state*)
  ; (let [channels (string/split channels #",")]
  ;   (reduce part-channel *state* channels)))

(defmethod message :NAMES
  [_ channel]
  *state*)

(defmethod message :MODE
  [_ channel]
  ; todo stub this out -- reply with 324 nick #channel +ntr, 329 nick #channel 0
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
