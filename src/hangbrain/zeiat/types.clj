(ns hangbrain.zeiat.types
  "Schema type definitions for the Zeiat library."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [hangbrain.zeiat.backend :as backend]
    ))

(defschema ZeiatBackend
  "Schema for objects implementing the Zeiat backend protocol. See zeiat.backend for details."
  (s/protocol backend/ZeiatBackend))

(defschema ClientState
  "The parts of a Zeiat agent state that contain information "
  "A Zeiat client consists of:
  - a client socket
  - a thread constantly reading messages from that socket
  - a translator agent that handles all message processing and socket writes
  "

(defschema IRCState
  "The current state of a connection to the IRC interface."

(defschema TranslatorState
  "The internal state of a Zeiat translator session.
  Structurally, a Zeiat session consists of:
  - an agent that holds the TranslatorState and manages access to the backend
  - a thread that constantly reads messages from the IRC connection, parses them, and sends them to the agent
  - a thread that periodically reminds the agent to check the backend for new messages
  "
  {; Connection to the IRC client; read by the reader thread, written by the agent.
   :socket java.net.Socket
   ; Writer wrapped around the socket so we can println to it.
   :writer (s/pred (partial instance? java.io.Writer))
   ; A future backed by the reader thread. When the reader thread exits, this will
   ; become realized.
   :reader (s/pred future?)
   ; Backend object, implementation-dependent
   ; TODO: move at least part of the channel cache from the backend to this structure,
   ; so that each backend doesn't have to re-implement it
   :backend ZeiatBackend
   ; Information about the state of the IRC client starts here.
   ; NICK/USER information -- used when generating PRIVMSG/JOIN events. Set once
   ; on connection and then fixed in place.
   ; TODO: move this to a backend/User struct so it can be directly compared
   :name (s/maybe s/Str)
   :user (s/maybe s/Str)
   :realname (s/maybe s/Str)
   ; Set of joined channels. The output of listUnread() will be intersected with this
   ; to determine which channels the client should get messages from.
   :channels #{s/Str}
   ; Negotiated capabilities. Not implemented yet.
   ; Goal is to support:
   ; - message-tags (required for other caps)
   ; - servertime (message timestamps)
   ; - echo-message (send confirmation)
   ; - batch (for BATCH CHATHISTORY message)
   ; - msgid tag (needed for edits/reactji)
   ;:cap #{s/Str}
   })

(defschema TranslatorAgent
  ; No validation for the agent interior as yet, but TranslatorState is installed as a validator when the agent is created, which should help.
  ; TODO: proper agent validation, perhaps by reifying the validator here and then checking that the agent's validator is == the one for TranslatorState
  (s/pred (partial instance? clojure.lang.Agent)))

