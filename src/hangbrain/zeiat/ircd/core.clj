(ns hangbrain.zeiat.ircd.core
  "Core functions for the ircd."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require [clojure.pprint :refer [pprint]])
  (:require
    [clojure.tools.logging :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [io.aviso.exception :refer [write-exception]]
    [clojure.string :as string]))
  ; (:import
  ;   [java.lang ArityException]))

(def ^:dynamic *state* nil)

(defn- fields-to-message
  [fields]
  (string/join
    " "
    (concat
      (butlast fields)
      [(str ":" (last fields))])))

(defn reply
  [& fields]
  (log/info "reply" *state*)
  (.println (:writer *state*) (fields-to-message fields))
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
  [state command argv]
  (binding [*state* state]
    (try
      (println "dispatching message" command argv)
      (apply message command argv)
      (println "message dispatched" command argv)
      (catch clojure.lang.ArityException e
        (log/warn "internal error handling command:")
        (write-exception e)
        (reply 461 "*" command "Wrong number of arguments for command")))))
