(ns hangbrain.zeiat.ircd.core
  "Core functions for the ircd."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require [clojure.pprint :refer [pprint]])
  (:require
    [clojure.tools.logging :as log]
    [hangbrain.zeiat.types :refer [TranslatorState TranslatorAgent]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [io.aviso.exception :refer [write-exception]]
    [clojure.string :as string]))
  ; (:import
  ;   [java.lang ArityException]))

(def ^:dynamic *state* nil)

(defn- fields-to-message
  [command fields]
  (string/join
    " "
    (concat
      [":Zeiat"
       (if (integer? command) (format "%03d" command) command)]
      (butlast fields)
      [(str ":" (last fields))])))

(defn reply
  [command & fields]
  (.println (:writer *state*) (fields-to-message command fields))
  *state*)

(defn join-channel
  [channel]
  *state*)

(defn part-channel
  [channel]
  *state*)

(defmulti message
  (fn [command & _rest]
    (-> command string/upper-case keyword)))

(defmethod message :default
  [command & rest]
  (reply 421 "*" command "Unrecognized command"))

(defn dispatch-message
  [state :- TranslatorState, command :- s/Str, & argv :- [s/Str]]
  (binding [*state* state]
    (try
      (log/trace "dispatching message" command argv)
      (apply message command argv)
      (log/trace "message dispatched" command argv)
      (catch clojure.lang.ArityException e
        (log/warn e "internal error handling command")
        (reply 461 "*" command "Wrong number of arguments for command")))))
