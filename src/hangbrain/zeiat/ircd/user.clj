(ns hangbrain.zeiat.ircd.user
  "User-management commands for the ircd interface."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* reply-from numeric]]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorState]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

;;; Client Management

(defn- registered?
  "True if the client connection is fully registered."
  [state]
  (and (:name state) (:user state)))

(defn- check-registration [state]
  (if (registered? state)
    (binding [*state* state]
      (numeric 1 "Welcome to the Zeiat IRC relay.")
      (numeric 5 "CHANTYPES=# NICKLEN=64 SAFELIST MAXTARGETS=1 LINELEN=8192")
      (numeric 376 "End of MOTD.")
      (translator/connect! state))
    state))

(defmethod message :NICK :- TranslatorState
  [_ nick]
  (if (registered? *state*)
    ; User already logged in, don't allow name changes post hoc
    (numeric 484 "NICK command restricted")
    ; Otherwise add it to the state
    (-> *state*
        (assoc :name nick)
        (check-registration))))

(defmethod message :USER :- TranslatorState
  [_ uname _mode _unused rname]
  (if (registered? *state*)
    (numeric 462 "Connection already registered.")
    (-> *state*
        (assoc :user uname)
        (assoc :realname rname)
        (check-registration))))

(defmethod message :PASS :- TranslatorState
  ; Password support. Not currently implemented.
  [_ _pass]
  *state*)

(defmethod message :CAP :- TranslatorState
  ; Capability negotiation. Currently we don't support this, which means we should just ignore it.
  ; TODO: implement timestamp and msgid capabilities
  [_ & _rest]
  *state*)

(defmethod message :PING :- TranslatorState
  [_ ping]
  (reply-from "Zeiat" "PONG" "Zeiat" ping))

(defmethod message :QUIT :- TranslatorState
  [_ reason]
  (translator/shutdown! *state* reason))
