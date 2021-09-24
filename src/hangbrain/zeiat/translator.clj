(ns hangbrain.zeiat.translator
  "The translator agent implementation. This handles processing commands and refreshing the backend."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd :as ircd]
    [hangbrain.zeiat.types :refer [TranslatorState TranslatorAgent]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    [clojure.java.io :as io]
    [clojure.tools.logging.readable :as log]
    )
  (:import
    [java.net Socket]
    [java.io BufferedReader PrintWriter]
    ))

(defn call
  [state key & argv]
  (apply (get-in state [:config key]) argv))

(defn dispatch-message
  [state command & argv]
  (log/trace "Dispatching message:" command argv)
  (ircd/dispatch-message state command argv))

(defn- handle-agent-error
  [agent error]
  (log/warn error "Error in agent" agent)
  ; TODO: instead of exiting, disconnect just that client and shut down the agent
  (System/exit 1))

(defn- reader-seq [reader]
  (take-while
    some?
    (repeatedly #(.readLine reader))))

(defn ^:private make-reader :- (s/pred future?)
  "Spawn a thread that endlessly reads messages from the socket, parses them, and sends them to the agent. Returns a future that will be realized when the socket is closed."
  [socket :- Socket, agent :- TranslatorAgent]
  (println "Creating socket reader:", socket)
  (let [reader (-> socket io/reader BufferedReader.)]
    (log/trace "reader created, creating future")
    (future
      (log/trace "socket reader, here we go!")
      (try
        (doseq [line (reader-seq reader)]
          (apply send agent
            dispatch-message (ircd/parse-line line)))
        (catch Exception e
          (log/error e)
          (.close socket)
          (shutdown-agents)
          (System/exit 1))))))

(defn- initialize-translator
  [state agent]
  ; Once we're done here it'll install the validator and then initialize the backend.
  (send agent
    (fn [state]
      (set-validator! agent (s/validator TranslatorState))
      (assoc state :backend (call state :connect))))
  (assoc state
    :reader (make-reader (:socket state) agent)))

(defn create :- TranslatorAgent
  "Create a translator agent with its state initialized to the client socket and backend config."
  [socket config]
  (let [agent (agent {:socket socket
                      :config config
                      :backend nil
                      :writer (-> socket io/writer (PrintWriter. true))}
                ; We do not install the validator here because we can't create the :reader field
                ; until after the agent has been created; so instead we partially create the agent
                ; state here, then finish creating it in initialize-agent, which also installs
                ; the validator.
                :error-mode :fail
                :error-handler handle-agent-error)]
   (log/trace "Creating translator agent:", agent)
   (send agent initialize-translator agent)))
