(ns hangbrain.zeiat.translator
  "The translator agent implementation. This handles processing commands and refreshing the backend."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.types :refer [TranslatorState]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [hangbrain.zeiat.ircd.core :refer [*state* privmsg]]
    [taoensso.timbre :as log]
    ))

(defn startup! :- TranslatorState
  "Called when the translator is created. Currently a no-op."
  [state :- TranslatorState]
  state)

(defn- interesting?
  [state chat]
  (or (= (:type chat) :dm)
    (contains? (:channels state) (:name chat))))

(defn- poll
  [{:keys [backend socket] :as state}]
  (if (.isClosed socket)
    (log/trace "poll thread exiting")
    (do
      (log/trace "polling for new messages...")
      (binding [*state* state]
        (->> (.listUnread (:backend state))
             (filter (partial interesting? state))
             (map :name)
             (mapcat #(.readNewMessages backend %))
             (run! privmsg)))
      (future
        (Thread/sleep 5000)
        (send *agent* poll))
      state)))

(defn connect! :- TranslatorState
  "Called when user registration completes successfully. Should connect to the backend."
  [state :- TranslatorState]
  (.connect (:backend state))
  (poll state))

(defn shutdown! :- TranslatorState
  ([state]
   (shutdown! state "(no reason given)"))
  ([{:keys [socket writer backend] :as state} :- TranslatorState, reason :- s/Str]
   (log/info "Shutting down translator:" socket "because:" reason)
   (when (not (.isClosed socket))
     (.println writer (str ":Zeiat ERROR :" reason))
     (.close socket))
   (.disconnect backend)
  state))
