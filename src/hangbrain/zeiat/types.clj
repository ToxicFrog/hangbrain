(ns hangbrain.zeiat.types
  "Schema type definitions for the Zeiat library."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [hangbrain.zeiat.backend :as backend]
    ))

(defschema ZeiatBackend
  (s/protocol backend/ZeiatBackend))

(defschema ClientState
  "A Zeiat client consists of:
  - a client socket
  - a thread constantly reading messages from that socket
  - a translator agent that handles all message processing and socket writes
  "
  {:socket java.net.Socket
   :backend ZeiatBackend
   :writer (s/pred (partial instance? java.io.Writer))
   :reader (s/pred future?)
   })

(defschema IRCState
  "The current state of a connection to the IRC interface."
  {; User identification
   :nick (s/maybe s/Str)
   :uname (s/maybe s/Str)
   :rname (s/maybe s/Str)
   ; Which channels have they joined?
   :channels #{s/Str}
   ; Should we autojoin them to channels with activity?
   :autojoin s/Bool
   })

(defschema TranslatorState
  (merge ClientState IRCState))

(defschema TranslatorAgent
  ; No validation for the agent interior as yet, but TranslatorState is installed as a validator when the agent is created, which should help.
  ; TODO: proper agent validation, perhaps by reifying the validator here and then checking that the agent's validator is == the one for TranslatorState
  (s/pred (partial instance? clojure.lang.Agent)))

