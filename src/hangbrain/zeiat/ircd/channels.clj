(ns hangbrain.zeiat.ircd.user
  "Channel-management commands for the ircd interface."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* numeric reply-from]]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorState]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

; TODO numerics should send the registered user's nick instead of * once registration is complete

; TODO: we need to figure out what the datashape of a chat really is
; I suspect something like: :name :ircname :type :id
; but perhaps ircname == id and it's up to the backend to keep track of their
; internal IDs, and the interface uses the ircname exclusively?
(defn- rpl-list
  [channel]
  (numeric 322 (:ircname channel) "0" (:name channel)))

(defmethod message :LIST
  [_ & _filter]
  ; TODO: implement filter support
  (as-> (.list (:backend *state*)) $
        (filter #(= :channel (:type %)) $)
        (run! rpl-list $))
  (numeric 323 "End of list."))

; on join, we need to reply with:
; :nick JOIN #foo
; 332 nick #foo :topic
; 353 nick = #foo :list of user names space separated
; 366 nick #foo :end of names
(defmethod message :JOIN
  [_ channels]
  *state*)
  ; (let [channels (string/split channels #",")]
  ;   (reduce join-channel *state* channels)))

(defmethod message :PART
  [_ channels _message]
  *state*)
  ; (let [channels (string/split channels #",")]
  ;   (reduce part-channel *state* channels)))

(defmethod message :WHO
  [_ _mask]
  *state*)

(defmethod message :NAMES
  [_ channel]
  *state*)

(defmethod message :MODE
  [_ channel]
  ; todo stub this out -- reply with 324 nick #channel +ntr, 329 nick #channel 0
  *state*)
