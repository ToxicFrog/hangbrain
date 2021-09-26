(require 'clojure.spec.alpha 'expound.alpha 'io.aviso.repl)
(io.aviso.repl/install-pretty-exceptions)

(ns hangbrain.core
  (:gen-class)
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [etaoin.api :as wd]
    [io.aviso.repl]
    [io.aviso.logging]
    [etaoin.keys :as keys]
    [taoensso.timbre :as log]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.tools.cli :as cli]
    [clojure.string :as string]
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

(defschema Channel
  {:type (s/enum :channel :dm)
   ; exact type of ID depends on the backend, for Hangouts this is a seq of
   ; element IDs that can be used to recover the clickable thing that selects
   ; the channel.
   :id s/Any
   ; will probably eventually need more elements here like channel name, last-read
   ; marker, etc.
   :name s/Str
   })

(defn el->selector
  [ctx el]
  (log/trace "getting id for element" el)
  (str
    "#"
    (string/replace
       (wd/get-element-attr-el ctx el :id)
       "/" "\\/")))

(defn maybe-get-attr
  [ctx root tag attr]
  (try
    (wd/get-element-attr ctx [root (str (name tag) "[" (name attr) "]")] attr)
    (catch Object _ nil)))

(defn ad-hoc-channel-name
  [ctx id]
  (log/trace "ad hoc channel name" id)
  (wd/get-element-attr ctx [id "span[role=presentation][title]"] :title))

(defmacro with-frame-el
  [ctx frame & body]
  `(try
     (wd/switch-frame* ~ctx (wd/el->ref ~frame))
     ~@body
     (finally (wd/switch-frame-parent ~ctx))))

(defmacro with-frame-n
  [ctx frame & body]
  `(try
     (wd/switch-frame* ~ctx ~frame)
     ~@body
     (finally (wd/switch-frame-parent ~ctx))))

(defn select-channel
  [ctx [iframe el]]
  (with-frame-el ctx iframe
    (wd/click-el ctx el)))

(defn send-message
  [ctx msg]
  (with-frame-n ctx 5
   (wd/fill ctx "div[spellcheck]" msg)
   (wd/fill ctx "div[spellcheck]" keys/enter)
  ))

; if we innerText a DM, the first line is going to be "Active", "Away", etc
; if there are unread messages, the second line will be "Unread"
; for DMs, inside the el, we're going to have a <span> with
;  data-hovercard-id=<email>
;  data-name=<human-readable name>
; timestamp is in span[data-absolute-timestamp]
; for ad hoc rooms there are going to be *multiple* spans with those elements,
; one per user in the room
; for channels, there's going to be a <span> with role=presentation title=<channel name>
(defn el->UserChannel
  [ctx iframe el]
  (let [id (el->selector ctx el)
        _ (log/trace "UserChannel" id)
        users (wd/query-all ctx [id "span[data-member-id]"])
        _ (log/trace "got user list")
        timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        _ (log/trace "got timestamp")]
    (cond
      (empty? users) nil
      (= 1 (count users)) ; DM
      {:id [iframe el]
       :name (wd/get-element-attr ctx [id "span[data-member-id]"] :data-name)
       :timestamp timestamp
       :type :dm}
      :else ; ad hoc channel
      {:id [iframe el]
       :name (ad-hoc-channel-name ctx id)
       :timestamp timestamp
       :type :channel}
      )))

(defn el->RoomChannel
  [ctx iframe el]
  (let [id (el->selector ctx el)
        timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        _ (log/trace "got timestamp" timestamp)]
    {:id [iframe el]
     :name (ad-hoc-channel-name ctx id)
     :timestamp timestamp
     :type :channel}))

(defn list-dms [ctx iframe]
  (log/trace "Getting chats in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->UserChannel ctx iframe))
         doall)))

(defn list-rooms [ctx iframe]
  (log/trace "Getting rooms in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->RoomChannel ctx iframe))
         doall)))

(defn list-chats [ctx]
  (let [[dm-iframe channel-iframe] (wd/query-all ctx "div[role=navigation] iframe")]
    (concat
      (list-dms ctx dm-iframe)
      (list-rooms ctx channel-iframe))))

(defn init-browser! [ctx]
  (doto ctx
    (wd/go "https://chat.google.com/")
    (wd/wait-exists "div#talk_roster" {:timeout 30 :interval 1})
    (wd/wait 1)))

(defn create-browser [opts]
  (wd/chrome
    {:args [(str "--user-data-dir=" (opts :profile))]
     :path-browser (opts :browser)
     :locator "css selector"}))

; (def config
;   (->> [:connect :disconnect :list-channels :list-users :list-members :list-unread :read-messages :write-message]
;        (map (fn [x] [x #(log/trace x %&)]))
;        (into {})))

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
    (zeiat/run (opts :listen-port) (stub/make-stub))))

(defn -main-old
  [& argv]
  (let [opts (parse-opts argv)
        ctx (create-browser opts)]
    (log/trace "OPTS" opts)
    (try
      (init-browser! ctx)
      (doall (map #(log/trace %&) (list-chats ctx)))
      (finally (wd/quit ctx)))))
  ; (let []
  ;   (with-chrome [ctx {;:profile (opts :profile)
  ;     (log/trace "OPTIONS" opts)
  ;     (log/trace "WEBDRIVER" ctx)
  ;     (init-browser! ctx)
  ;     (log/trace (list-chats ctx)))))
