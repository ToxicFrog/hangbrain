(ns hangbrain.zeiat.types
  "Schema type definitions for the Zeiat library."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
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

(defprotocol ZeiatBackend
  "A protocol that Zeiat uses to communicate with whatever backend you connect to it. Library users should supply something that implements this protocol to zeiat/run."
  (connect [this]
    "Connect to the backend. Called when a new user connects to Zeiat.")
  (disconnect [this]
    "Disconnect from the backend. Called when a user quits or is otherwise disconnected.")
  (list [this]
    "List all available chats. See the Chat schema for the data shape. Called in response to a user's LIST or WHO command.")
  (list-unread [this]
    "List all chats with unread messages. Called periodically to monitor for message traffic. Implementations can implement this however they want but note that just blindly returning all chats will result in a lot of unnecessary read-messages calls.")
  (list-members [this channel]
    "List all users in a given channel. See FIXME for the data shape. Called in response to NAMES or JOIN.")
  (read-messages [this channel]
    "Return all messages from the given channel available in the backscroll. Implementors can limit this to only what's easily available if convenient (e.g. return only history that was autoloaded, not all history). Calling this should mark the chat as read.")
  (write-message [this channel message]
    "Send a message to the given channel or user. Calling this should mark the chat as read."))

(defschema TranslatorState
  "A Zeiat client consists of:
  - a client socket
  - a thread constantly reading messages from that socket
  - a translator agent that handles all message processing and socket writes
  "
  {:socket java.net.Socket
   :backend (s/protocol ZeiatBackend)
   :writer (s/pred (partial instance? java.io.Writer))
   :reader (s/pred future?)
   })

(defschema TranslatorAgent
  ; No validation for the agent interior as yet, but TranslatorState is installed as a validator when the agent is created, which should help.
  ; TODO: proper agent validation, perhaps by reifying the validator here and then checking that the agent's validator is == the one for TranslatorState
  (s/pred (partial instance? clojure.lang.Agent)))

(defschema IRCState
  "The current state of a connection to the IRC interface."
  {; User identification
   :nick s/Str
   :uname s/Str
   :rname s/Str
   ; Which channels have they joined?
   :channels #{s/Str}
   ; Should we autojoin them to channels with activity?
   :autojoin s/Bool
   })
