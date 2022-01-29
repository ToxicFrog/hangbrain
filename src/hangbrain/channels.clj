(ns hangbrain.channels
  "Functions for listing and getting channel info."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [clojure.java.io :as io]
    [clojure.string :as string]
    [etaoin.api :as wd]
    [hangbrain.util :as util]
    [io.aviso.repl]
    [io.aviso.logging]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [taoensso.timbre :as log])
  )

(defn ->IRCChannel
  [name]
  (as-> name $
      (string/trim $)
      (string/replace $ #", +", "+")
      (string/replace $ #"[ ,]+", "-")
      (str "#" $)))

(defn- ->ChatInfo [info]
  ; if we're generating the info for a channel this doesn't set up :users right
  (let [info (update info :type keyword)
        ts (when (:timestamp info) (util/millis->datetime (:timestamp info)))
        name (case (:type info)
               :dm (util/->IRCNick (:realname info))
               :channel (->IRCChannel (:topic info)))]
    (-> info
        (assoc :name name :timestamp ts)
        (update :users
          (fn [users] (->> users
                           (map #(assoc % :type :dm))
                           (map ->ChatInfo))))
        )))

(defn- add-id-to-name [chat]
  (assoc chat :name
    (str (:name chat)
      "["
      (-> chat :user (string/split #"@") first)
      "]")))

(defn- dedup-one-name [chats]
  (cond
    (= 1 (count chats)) chats
    :else (map add-id-to-name chats)))

(defn- deduplicate-chats [chats]
  (->> chats
       (group-by :name)
       (vals)
       (mapcat dedup-one-name)))

(def js-list-user-channels
  (-> "list-user-channels.js" io/resource slurp))

(defn list-dms [ctx iframe]
  (log/trace "Getting chats in iframe" iframe)
  (util/with-frame-el ctx iframe
    (->> (wd/js-execute ctx js-list-user-channels)
         (map ->ChatInfo)
         (deduplicate-chats)
         )))

(def js-list-room-channels
  (-> "list-room-channels.js" io/resource slurp))

(defn list-rooms [ctx iframe]
  (log/trace "Getting rooms in iframe" iframe)
  (util/with-frame-el ctx iframe
    (->> (wd/js-execute ctx js-list-room-channels)
         (map ->ChatInfo)
         )))

(defn list-all [ctx]
  (let [[dm-iframe channel-iframe] (wd/query-all ctx "div[role=navigation] iframe")]
    (concat
      (list-rooms ctx channel-iframe)
      (list-dms ctx dm-iframe)
      )))
