(ns hangbrain.zeiat.ircd.list
  "Channel-management commands for the ircd interface."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* numeric]]
    [hangbrain.zeiat.backend :as backend]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

; TODO: we need to figure out what the datashape of a chat really is
; I suspect something like: :name :ircname :type :id
; but perhaps ircname == id and it's up to the backend to keep track of their
; internal IDs, and the interface uses the ircname exclusively?
(defn- rpl-list
  [{:keys [name count topic]}]
  (numeric 322 name count topic))

(defmethod message :LIST
  [_ & _filter]
  ; TODO: implement filter support
  (numeric 321 "Channel" "Users Name")
  (as-> (backend/list-channels (:backend *state*)) $
        (run! rpl-list $))
  (numeric 323 "End of LIST"))

(defn- rpl-who
  [{:keys [name user host realname]}]
  (numeric 352 "*" name host "Zeiat" user "H@" (str "0 " realname)))

(defmethod message :WHO
  ([_ & _filters]
  ; TODO implement filter support -- many clients will send a WHO <channel> as soon
  ; as a channel is successfully joined, so this is actually important.
  (as-> (backend/list-users (:backend *state*)) $
        (run! rpl-who $))
  (numeric 315 "End of WHO")))
