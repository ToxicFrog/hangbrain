(ns hangbrain.zeiat.translator
  "The translator agent implementation. This handles processing commands and refreshing the backend."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd :as ircd]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    [clojure.java.io :as io]
    [clojure.tools.logging.readable :as log]
    )
  (:import
    [java.net Socket]
    [java.io BufferedReader]
    ))

(defn call
  [state key & argv]
  (apply (get-in state [:config key]) argv))

(defn dispatch-message
  [state command & argv]
  (log/trace "Dispatching message:" command argv)
  (ircd/dispatch-message state command argv))

(defn- initialize
  [state]
  (assoc state
    :backend (call state :connect)))

(defn- handle-agent-error
  [agent error]
  (log/warn error "Error in agent" agent)
  ; TODO: instead of exiting, disconnect just that client and shut down the agent
  (System/exit 1))

; TODO we should install an error handler so that on error we immediately exit
; and disconnect the client, rather than detecting the error only the next time
; we try to talk to the agent.
(defn create
  "Create a translator agent with its state initialized to the client socket and backend config."
  [socket config]
  (let [agent (agent
                {:socket socket, :config config}
                :error-mode :fail
                :error-handler handle-agent-error)]
   (log/trace "Creating translator agent:", agent)
    (send agent initialize)))
