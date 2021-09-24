(ns hangbrain.zeiat
  "A library for connecting IRC to other protocols. Zeiat only handles the IRC frontend; it's up to you to provide a backend for it to connect to."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.tools.logging.readable :as log]
    [clojure.string :as string]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorAgent BackendConfiguration]]
    [clojure.java.io :as io]
    )
  (:import
    [java.net Socket ServerSocket]
    [java.io PrintWriter]
    ))

(defn run :- [TranslatorAgent]
  "Open a listen socket and loop accepting clients from it and creating a new Zeiat instance for each client. Continues until the socket is closed, then returns a seq of all still-connected clients."
  [listen-port :- s/Int, config :- BackendConfiguration]
  (let [sock (ServerSocket. listen-port)]
    (loop [clients []]
      (if (not (.isClosed sock))
        (recur
          ; TODO: once we can detect dead clients, we should filter them from the list here
          ; so they don't endlessly pile up.
          (conj clients (translator/create (.accept sock) config)))
        clients))))

; TODO: api functions for interacting with agents, like shutdown and running?
