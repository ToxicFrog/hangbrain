(ns hangbrain.zeiat.ircd.core
  "Core functions for the ircd."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require [clojure.pprint :refer [pprint]])
  (:require
    [taoensso.timbre :as log]
    [hangbrain.zeiat.types :refer [TranslatorState TranslatorAgent]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [io.aviso.exception :refer [write-exception]]
    [clojure.string :as string]))

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
  (.println (:writer *state*) (fields-to-message fields))
  *state*)

(defn reply-from
  [name & fields]
  (apply reply (str ":" name) fields))

(defn numeric
  [num & fields]
  (.println
    (:writer *state*)
    (fields-to-message
      (concat [":Zeiat" (format "%03d" num) (:name *state* "*")] fields)))
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
      (catch clojure.lang.ArityException e
        (log/warn e "internal error handling command")
        (numeric 461 command "Wrong number of arguments for command")))))

(defn- irctarget
  [who]
  (if (= :me who)
    (:name *state*)
    (:name who)))

(defn privmsg
  [msg]
  (log/trace "privmsg" msg)
  (->> (string/split-lines (:text msg))
       (map string/trim)
       (filter (complement empty?))
       (run! (partial reply-from (irctarget (:from msg)) "PRIVMSG" (irctarget (:to msg))))))
