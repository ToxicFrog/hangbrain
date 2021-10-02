(ns hangbrain.zeiat.ircd.messages
  "Commands for sending and receiving messages."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.ircd.core :as ircd :refer [message *state* numeric reply-from privmsg]]
    [hangbrain.zeiat.translator :as translator]
    [hangbrain.zeiat.types :refer [TranslatorState]]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    ))

(defmethod message :PRIVMSG
  [_ channel msg]
  ; TODO: echo the sent message back to the client when it appears, if the client
  ; has negotiated the echo capability
  ; TODO: handle CTCP properly, especially CTCP ACTION
  (if (.writeMessage (:backend *state*) channel msg)
    *state*
    (if (string/starts-with? channel "#")
      (numeric 403 channel "No such channel")
      (numeric 401 channel "No such user"))))

(defmethod message :RECAP
  [_ channel]
  (let [recap (.readMessages (:backend *state*) channel)]
    (log/trace "Done fetching RECAP, total message count:" (count recap))
    (if (nil? recap)
      (numeric 403 channel "No such user/channel")
      (do (run! privmsg recap) *state*))))
