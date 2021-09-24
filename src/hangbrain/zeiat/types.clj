(ns hangbrain.zeiat.types
  "Schema type definitions for the Zeiat library."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    )
  ; (:import
  ;   [java.net Socket]
  ;   [java.io Writer])
    )

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

(defschema TranslatorState
  "A Zeiat client consists of:
  - a client socket
  - a thread constantly reading messages from that socket
  - a translator agent that handles all message processing and socket writes
  "
  {:socket java.net.Socket
   :config BackendConfiguration
   :backend BackendState
   :writer (s/pred (partial instance? java.io.Writer))
   :reader (s/pred future?)
   })

(defschema TranslatorAgent
  ; No validation for the agent interior as yet, but TranslatorState is installed as a validator when the agent is created, which should help.
  (s/pred (partial instance? clojure.lang.Agent)))

