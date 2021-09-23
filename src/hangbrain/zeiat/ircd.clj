(ns hangbrain.zeiat.ircd
  "Proxy module that loads all the modules making up the IRCD engine and exports the 'dispatch-message function to send events to it."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd-core]
    [hangbrain.zeiat.ircd.commands]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

(intern *ns* 'dispatch-message ircd-core/dispatch-message)
; (println ircd-core/dispatch-message)
; (println dispatch-message)
