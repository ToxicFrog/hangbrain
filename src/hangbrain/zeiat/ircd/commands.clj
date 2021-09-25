(ns hangbrain.zeiat.ircd.commands
  "Command handlers for the ircd."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* reply]]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorState]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

;;; Client Management

(defn- registered?
  "True if the client connection is fully registered."
  [state]
  (and (:nick state) (:uname state)))

(defn- check-registration [state]
  (if (registered? state)
    (do
      (reply 1 "*" "Welcome to the Zeiat IRC relay.")
      (reply 376 "*" "End of MOTD."))
    state))

(defmethod message :NICK :- TranslatorState
  [_ nick]
  (if (registered? *state*)
    ; User already logged in, don't allow name changes post hoc
    (reply 484 "*" "NICK command restricted")
    ; Otherwise add it to the state
    (-> *state*
        (assoc :nick nick)
        (check-registration))))

(defmethod message :USER :- TranslatorState
  [_ uname _mode _unused rname]
  (if (registered? *state*)
    (reply 462 "*" "Connection already registered.")
    (-> *state*
        (assoc :uname uname)
        (assoc :rname rname)
        (check-registration))))

(defmethod message :PASS :- TranslatorState
  ; Password support. Not currently implemented.
  [_ _pass]
  *state*)

(defmethod message :CAP :- TranslatorState
  ; Capability negotiation. Currently we don't support this, which means we should just ignore it.
  ; TODO: implement timestamp and msgid capabilities
  [_ & rest]
  *state*)

(defmethod message :PING :- TranslatorState
  [_ ping]
  (reply "PONG" "Zeiat" ping))

(defmethod message :QUIT :- TranslatorState
  [_ _message]
  ; this is annoying because we need a way to send to the agent, but this
  ; introduces a circular dependency on zeiat.translator
  ; perhaps we need another library, zeiat.translator.api or such
  (translator/shutdown!))

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

;;; Zeiat extensions

(defmethod message :RECAP
  [_ & channels]
  ; Replay chat for the named channels/users
  *state*)

(defmethod message :AUTOJOIN
  [_ onoff]
  ; if ON, automatically JOIN the user to channels with unread chatter
  ; if OFF, don't (i.e. behave like a normal IRC server)
  *state*)
