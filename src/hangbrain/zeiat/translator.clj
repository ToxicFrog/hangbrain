(ns hangbrain.zeiat.translator
  "The translator agent implementation. This handles processing commands and refreshing the backend."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.types :refer [TranslatorState TranslatorAgent]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.tools.logging.readable :as log]
    ))

(defn- call
  [state key & argv]
  (apply (get-in state [:config key]) argv))

(defn startup! :- TranslatorState
  [state :- TranslatorState]
  (assoc state :backend (call state :connect)))

(defn shutdown! :- TranslatorState
  [state :- TranslatorState]
  (.close (:socket state))
  (assoc state :backend (call state :disconnect)))
