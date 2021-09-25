(ns hangbrain.zeiat
  "A library for connecting IRC to other protocols. Zeiat only handles the IRC frontend; it's up to you to provide a backend for it to connect to."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.tools.logging.readable :as log]
    [clojure.string :as string]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.ircd :as ircd]
    [hangbrain.zeiat.types :refer [TranslatorAgent BackendConfiguration]]
    [clojure.java.io :as io]
    )
  (:import
    [java.net Socket ServerSocket]
    [java.io PrintWriter BufferedReader]
    ))

(defn- handle-agent-error
  [agent error]
  (log/warn error "Error in agent" agent)
  (translator/shutdown!))

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
            ircd/dispatch-message (ircd/parse-line line)))
        (catch Exception e
          (log/error e)
          (.close socket)
          (shutdown-agents)
          (System/exit 1))))))

(defn- initialize-translator
  [state agent]
  ; Once we're done here it'll install the validator and then initialize the backend.
  (send agent translator/startup!)
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

(defn run :- [TranslatorAgent]
  "Open a listen socket and loop accepting clients from it and creating a new Zeiat instance for each client. Continues until the socket is closed, then returns a seq of all still-connected clients."
  [listen-port :- s/Int, config :- BackendConfiguration]
  (let [sock (ServerSocket. listen-port)]
    (log/info "Listening for connections on port" listen-port)
    (loop [clients []]
      (if (not (.isClosed sock))
        (recur
          ; TODO: once we can detect dead clients, we should filter them from the list here
          ; so they don't endlessly pile up.
          (conj clients (create (.accept sock) config)))
        clients))))

(defn running? :- s/Bool
  [agent :- TranslatorAgent]
  (realized? (:reader @agent)))
