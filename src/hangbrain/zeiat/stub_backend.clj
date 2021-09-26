(ns hangbrain.zeiat.stub-backend
  "A stub implementation of ZeiatBackend, for testing."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [taoensso.timbre :as log]
    [hangbrain.zeiat.types :refer [TranslatorAgent ZeiatBackend]]
    ))

(defn make-stub :- (s/protocol ZeiatBackend)
  []
  (reify ZeiatBackend
    (connect [this]
      (log/trace "stub: connect" this))
    (disconnect [this]
      (log/trace "stub: disconnect" this))
    (list [this]
      (log/trace "stub: list" this)
      [])
    (list-unread [this]
      (log/trace "stub: list-unread" this)
      [])
    (list-members [this channel]
      (log/trace "stub: list-members" this channel)
      [])
    (read-messages [this channel]
      (log/trace "stub: read-messages" this channel)
      [])
    (write-message [this channel message]
      (log/trace "stub: write-message" this channel message))))
