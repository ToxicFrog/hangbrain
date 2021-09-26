(require 'clojure.spec.alpha 'expound.alpha 'io.aviso.repl)
(io.aviso.repl/install-pretty-exceptions)

(ns hangbrain.core
  (:gen-class)
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [io.aviso.repl]
    [io.aviso.logging]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.tools.cli :as cli]
    [clojure.string :as string]
    [hangbrain.backend :as backend]
    [hangbrain.zeiat :as zeiat]
    [hangbrain.zeiat.stub-backend :as stub]
    ))

(io.aviso.repl/install-pretty-exceptions)
(io.aviso.logging/install-pretty-logging)
(io.aviso.logging/install-uncaught-exception-handler)

(def log-levels
  {:trace  "\u001B[96mTRC\u001B[0m"
   :debug  "\u001B[94mDBG\u001B[0m"
   :info   "INF"
   :warn   "\u001B[93mWRN\u001B[0m"
   :error  "\u001B[91mERR\u001B[0m"
   :fatal  "\u001B[91mFTL\u001B[0m"
   :report "RPT"})

(defn init-logging! [log-level]
  (log/set-level! (keyword log-level))
  (log/merge-config!
    {:output-fn
     (fn logger [data]
       (let [{:keys [level #_vargs msg_ ?ns-str ?file timestamp_ ?line ?err]} data]
         (str
           (log-levels level) " "
           @timestamp_ " "
           "[" (or ?ns-str ?file "?") ":" (or ?line "?") "] - "
           @msg_
           (if ?err
             (str "\n" (log/stacktrace ?err))))))
     :timestamp-opts {:pattern "HH:mm:ss"}}))

(def flags
  [["-l" "--listen-port PORT" "Port to listen for IRC connections on"
    :default 4000
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
   ["-L" "--log-level LEVEL" "Log level"
    :default :info
    :parse-fn #(keyword (string/lower-case %))
    :validate
    [#{:trace :debug :info :warn :error :fatal :report}
     "Must be one of trace/debug/info/warn/error/fatal/report"]]
   ["-p" "--profile PROFILE_DIR"
    "Browser profile directory. Must already be signed in to Chat!"
    :default (str (get (System/getenv) "HOME") "/.config/hangbrain")]
   ["-b" "--browser BROWSER_PATH"
    "Path to Chrome or Chromium"
    :default "/usr/bin/chromium"]
   ["-h" "--help" "This text"]])

(defn parse-opts [argv]
  (let [{:keys [options _argv errors summary]} (cli/parse-opts argv flags)]
    (cond
      (:help options) (do
                        (log/info summary)
                        (System/exit 0))
      errors (do
               (binding [*out* *err*] (dorun (map #(log/error %) errors))
               (log/trace summary)
               (System/exit 1))))
    options))

(log/trace "compiling main")
(defn -main
  [& argv]
  (println "Starting up...")
  (s/set-fn-validation! true)
  (io.aviso.repl/install-pretty-exceptions)
  (io.aviso.logging/install-pretty-logging)
  (io.aviso.logging/install-uncaught-exception-handler)
  (let [opts (parse-opts argv)]
    (init-logging! (opts :log-level))
    (zeiat/run (opts :listen-port)
               (backend/create opts))))
               ;(stub/make-stub))))
