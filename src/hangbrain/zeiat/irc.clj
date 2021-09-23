(ns hangbrain.zeiat.irc
  "The IRC interface for Zeiat. This handles reading IRC messages from the client and parsing and dispatching them."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [hangbrain.zeiat.translator :as translator]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string]
    [clojure.java.io :as io]
    )
  (:import
    [java.net Socket]
    [java.lang Exception]
    [java.io BufferedReader]
    ))

; IRC message fields are all whitespace separated.
; A message consists of:
; - an optional prefix field, starting with :
; - a command field, which is either a three-digit number or a textual command such as PRIVMSG
; - zero or more arguments
; If an argument starts with :, that means it is the final argument and consumes the rest of the line.
; This is how arguments containing whitespace are sent.

(defn- extract-prefix
  [message]
  (if (= \: (first message))
    ; prefix -- split into prefix and tail, drop : from prefix
    (update (string/split message #" " 2)
      0 string/replace-first ":" "")
    ; no prefix
    [nil message]))

(defn- next-message-field
  [message]
  (if (= \: (first message))
    ; field starts with :, all the rest of the message is the field
    [(string/replace-first message ":" "") nil]
    ; no leading :, eat the field up to the next space
    (string/split message #" " 2)))

(defn- parse-irc-message
  "Parse an IRC message into a prefix, command, and args."
  [message]
  (println "parse message" message)
  (let [[_prefix message] (extract-prefix message)]
    (loop [message message fields []]
      (if (nil? message)
        fields
        (let [[field message] (next-message-field message)]
          (recur message (conj fields field)))))))

(defn- parse-and-dispatch
  [agent line]
  (println "parse-and-dispatch" line)
  (apply send agent translator/dispatch-message (parse-irc-message line)))

(defn- reader-seq [reader]
  (take-while
    some?
    (repeatedly #(.readLine reader))))

(defn make-reader :- (s/pred future?)
  "Spawn a thread that endlessly reads messages from the socket, parses them, and sends them to the agent. Returns a future that will be realized when the socket is closed."
  [socket :- Socket, agent]
  (println "Creating socket reader:", socket)
  (let [reader (-> socket io/reader BufferedReader.)]
    (println "reader created, creating future")
    (future
      (println "socket reader, here we go!")
      (try
        (doseq [line (reader-seq reader)]
          (parse-and-dispatch agent line))
        (catch Exception e
          (println e)
          (.close socket)
          (shutdown-agents)
          (System/exit 1))))))
