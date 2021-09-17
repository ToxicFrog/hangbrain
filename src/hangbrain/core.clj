(ns hangbrain.core
  (:gen-class)
  (:require
    [etaoin.api :as wd]
    [etaoin.keys :as keys]
    ; [etaoin.api2 :as wd2 :refer [with-chrome]]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [slingshot.slingshot :refer [try+]]
    [clojure.tools.cli :as cli]
    [clojure.string :as string]
    ))

(def flags
  [["-l" "--listen-port PORT" "Port to listen for IRC connections on"
    :default 4000
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 0x10000) "Must be a number between 0 and 65536"]]
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
                        (println summary)
                        (System/exit 0))
      errors (do
               (binding [*out* *err*] (dorun (map println errors))
               (println summary)
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
  (println "getting id for element" el)
  (str
    "#"
    (string/replace
       (wd/get-element-attr-el ctx el :id)
       "/" "\\/")))

(defn maybe-get-attr
  [ctx root tag attr]
  (try+
    (wd/get-element-attr ctx [root (str (name tag) "[" (name attr) "]")] attr)
    (catch Object _ nil)))

(defn ad-hoc-channel-name
  [ctx id]
  (println "ad hoc channel name" id)
  (wd/get-element-attr ctx [id "span[role=presentation][title]"] :title))

(defmacro with-frame-el
  [ctx frame & body]
  `(try+
     (wd/switch-frame* ~ctx (wd/el->ref ~frame))
     ~@body
     (finally (wd/switch-frame-parent ~ctx))))

(defmacro with-frame-n
  [ctx frame & body]
  `(try+
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
        _ (println "UserChannel" id)
        users (wd/query-all ctx [id "span[data-member-id]"])
        _ (println "got user list")
        timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        _ (println "got timestamp")]
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
        _ (println "got timestamp" timestamp)]
    {:id [iframe el]
     :name (ad-hoc-channel-name ctx id)
     :timestamp timestamp
     :type :channel}))

(defn list-dms [ctx iframe]
  (println "Getting chats in iframe" iframe)
  (with-frame-el ctx iframe
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->UserChannel ctx iframe))
         doall)))

(defn list-rooms [ctx iframe]
  (println "Getting rooms in iframe" iframe)
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

; IRC interface
; LIST returns a list of IRC-compatible channel name and user count pairs
(defn LIST [] nil)
; NAMES [channel] returns a list of users on the channel
; this is going to be hard because it involves clicking the channel menu, then
; "list members", then parsing the result of that, then dismissing the modal
(defn NAMES [] nil)
; WHO lists all users reachable by DM
; it returns nick, user, host, server, and real name (among other things),
; which we should probably map to: munged name, @ prefix, @ suffix, "hangouts",
; and display name
(defn WHO [] nil)

(defn -main
  [& argv]
  (let [opts (parse-opts argv)
        ctx (create-browser opts)]
    (println "OPTS" opts)
    (try+
      (init-browser! ctx)
      (doall (map println (list-chats ctx)))
      (finally (wd/quit ctx)))))
  ; (let []
  ;   (with-chrome [ctx {;:profile (opts :profile)
  ;     (println "OPTIONS" opts)
  ;     (println "WEBDRIVER" ctx)
  ;     (init-browser! ctx)
  ;     (println (list-chats ctx)))))
