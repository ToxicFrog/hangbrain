(ns hangbrain.zeiat.stub-backend
  "A stub implementation of ZeiatBackend, for testing."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [taoensso.timbre :as log]
    [hangbrain.zeiat.backend :refer [ZeiatBackend]]
    ))

(defn make-stub :- (s/protocol ZeiatBackend)
  []
  (reify ZeiatBackend
    (connect [this]
      (log/trace "stub: connect" this))
    (disconnect [this]
      (log/trace "stub: disconnect" this))
    (listChannels [this]
      (log/trace "stub: list" this)
      [])
    (statChannel [this channel])
    (listUsers [this]
      (log/trace "stub: list" this)
      [])
    (listUnread [this]
      (log/trace "stub: list-unread" this)
      [])
    (listMembers [this channel]
      (log/trace "stub: list-members" this channel)
      [])
    (readMessages [this channel]
      (log/trace "stub: read-messages" this channel)
      [])
    (readNewMessages [this channel]
      (log/trace "stub: read-new-messages" this channel)
      [])
    (writeMessage [this channel message]
      (log/trace "stub: write-message" this channel message))))
