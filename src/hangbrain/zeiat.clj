(ns hangbrain.zeiat
  "A library for connecting IRC to other protocols. Zeiat only handles the IRC frontend; it's up to you to provide a backend for it to connect to."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    [hangbrain.zeiat.irc :as irc]
    [hangbrain.zeiat.translator :as translator]
    [clojure.java.io :as io]
    )
  (:import
    [java.net Socket ServerSocket]
    [java.io PrintWriter]
    ))

(defschema BackendState s/Any)

(defschema Chat s/Any)

(defschema Message s/Any)

(defschema BackendConfiguration
  "A map of functions used to interact with the backend."
  {; Connect to the backend and return a handle for it, or disconnect a running one.
   :connect (s/=> BackendState)
   :disconnect (s/=> s/Any BackendState)
   ; The former can be joined, the latter must be DMed
   :list-channels (s/=> [Chat] BackendState)
   :list-users (s/=> [Chat] BackendState)
   ; List all the names associated with a chat. For DMs this is identity. For channels
   ; it should return a Chat structure for each user in the channel.
   :list-members (s/=> [Chat] BackendState Chat)
   ; List all chats with unread content.
   ; TODO: we may want to be able to pass in a map of "interesting" chats, which the
   ; backend can use as an optimization.
   :list-unread (s/=> [Chat] BackendState)
   ; Return a list of messages from the given chat.
   :read-messages (s/=> [Message] BackendState Chat)
   ; Send a message to the given chat.
   :write-message (s/=> s/Any BackendState Chat s/Str)
   })

; (defschema BackendConfiguration
;   {:connect (s/=> BackendState)
;    :disconnect (s/=> s/Any BackendState)
;    :list-channels s/Any
;    :list-users s/Any
;    :list-members s/Any
;    :list-unread s/Any
;    :read-messages s/Any
;    :write-message s/Any})

(defschema Client
  "A Zeiat client consists of:
  - a client socket
  - a thread constantly reading messages from that socket
  - a translator agent that handles all message processing and socket writes
  "
  {:socket Socket
   :writer s/Any ; clojure io/writer
   :agent s/Any ; should be (pred agent?) or so, but there's no agent? fn
   :future (s/pred future?)
   })

(defn create :- Client
  "Create a new Zeiat client bound to the given TCP socket."
  [socket :- Socket, config :- BackendConfiguration]
  (println "Creating new client:", socket)
  (let [agent (translator/create socket config)]
    {:socket socket,
     :writer (-> socket io/writer (PrintWriter. true))
     :agent agent,
     :future (irc/make-reader socket agent)}))

; (defn run [& rest] nil)

(defn run :- [Client]
  "Open a listen socket and loop accepting clients from it and creating a new Zeiat instance for each client. Continues until the socket is closed, then returns a seq of all still-connected clients."
  [listen-port :- s/Int, config :- BackendConfiguration]
  (println listen-port (type listen-port))
  (let [sock (ServerSocket. listen-port)]
    (loop [clients []]
      (if (not (.isClosed sock))
        (recur
          ; TODO: once we can detect dead clients, we should filter them from the list here
          ; so they don't endlessly pile up.
          (conj clients (create (.accept sock) config)))
        clients))))

(println run)
(println (s/fn-schema run))
(println (s/explain (s/fn-schema run)))
