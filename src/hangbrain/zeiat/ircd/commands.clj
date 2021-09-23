(ns hangbrain.zeiat.ircd.commands
  "Command handlers for the ircd."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* reply]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

(defn check-registration [state]
  state)

(println "registering message handlers" message)

;;; User Management

(defmethod message :NICK
  [_ nick]
  ; TODO reject this if the user is already signed in
  (cond
    ; Reject if the user is already logged in
    (*state* :nick)
    (reply 437 nick ":Unavailable")
    :else
    (-> *state*
        (assoc :nick nick)
        (check-registration))))

(defmethod message :USER
  [_ uname _mode _unused rname]
  (-> *state*
      (assoc :uname uname)
      (assoc :rname rname)
      (check-registration)))

; (defmethod message :PASS
;   [_ pass]
;   (if (or (nil? (*state* :pass))
;         (= (*state* :pass) pass))
;     (-> *state* (dissoc :pass) (check-registration))
;     )

(defmethod message :CAP
  [_ & rest]
  ; TODO: implement capability negotiation
  ; we want timestamp and msgid at minimum
  )

(defmethod message :PING
  [_ ping]
  (reply :pong "Zeiat" ping))

(defmethod message :QUIT
  [_ message]
  ; disconnect user and shut down thread
  )

;;; Channel Management

(defmethod message :JOIN
  [_ channels]
  (let [channels (string/split channels #",")]
    (reduce ircd/join-channel *state* channels)))

(defmethod message :PART
  [_ channels _message]
  (let [channels (string/split channels #",")]
    (reduce ircd/part-channel *state* channels)))

(defmethod message :WHO
  [_ _mask]
  *state*)

(defmethod message :NAMES
  [_ channel]
  *state*)

(defmethod message :MODE
  [_ channel]
  ; todo stub this out -- reply with 324 nick #channel +ntr, 329 nick #channel 0
  *state*)

;;; Talking

(defmethod message :PRIVMSG
  [_ channel msg]
  ; TODO: make sure to handle CTCP properly!
  *state*)
