(ns hangbrain.core
  (:gen-class)
  (:require
    [etaoin.api :as wd]
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
    :default (str (get (System/getenv) "HOME") "/.config/hangbrain-chrome")]
   ["-b" "--browser BROWSER_PATH"
    "Path to Chrome or Chromium"
    :default "chromium"]
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

; (defn el->Channel :- Channel
;   [ctx iframe el]
;   {:type :dm
;    :id [iframe el]
;    :name (get-element-text-el ctx el)
;    })

; (defschema TagId s/Str)

(defn el->id
  [ctx el]
  (println "getting id for element" el)
  (wd/get-element-attr-el ctx el :id))

; (defn list-chats-in-iframe :- [Channel]
;   [ctx iframe]
;   (println "Getting chats in iframe" iframe)
;   (switch-frame* ctx (el->ref iframe))
;   (let [chats (map (partial el->Channel ctx iframe)
;                 (query-all ctx "span[role=listitem]"))]
;     (println "got channels:" chats)
;     (switch-frame-top ctx)
;     chats))
  ; ; returns only the first
  ; (let [chats (get-element-text ctx "span[role=listitem]")]
  ;   (println "got chats with internal element ids" chats)
  ;   (switch-frame-top ctx)
  ;   chats))

; (def ^:dynamic *browser* nil)

; (defn chat-info [ctx chat]
;   chat)
  ; (println "getting chat info for" chat)
  ; (let [text (get-element-text-el ctx (el->ref chat))]
  ;   (println " -> " text)
  ;   text))

; (defn ad-hoc-channel-name
;   [ctx id users]
;   (->> users
;     (map #(wd/get-element-attr-el ctx % :data-name))
;     (apply str)
;     ))

(defn maybe-get-attr
  [ctx root tag attr]
  (try+
    (wd/get-element-attr ctx [root (str (name tag) "[" (name attr) "]")] attr)
    (catch Object _ nil)))

(defn ad-hoc-channel-name
  [ctx id]
  (println "ad hoc channel name" id)
  (wd/get-element-attr ctx [id "span[role=presentation][title]"] :title))

; for DMs, inside the el, we're going to have a <span> with
;  data-hovercard-id=<email>
;  data-name=<human-readable name>
; timestamp is in span[data-absolute-timestamp]
; for ad hoc rooms there are going to be *multiple* spans with those elements,
; one per user in the room
; for channels, there's going to be a <span> with role=presentation title=<channel name>
(defn id->UserChannel
  [ctx id]
  (let [id (string/replace (str "#" id) "/" "\\/")
        _ (println "UserChannel" id)
        users (wd/query-all ctx [id "span[data-member-id]"])
        _ (println "got user list")
        timestamp (maybe-get-attr ctx id :span :data-absolute-timestamp)
        _ (println "got timestamp")]
    (println "userchannel" id timestamp (count users) users)
    (cond
      (empty? users) nil
      (= 1 (count users)) ; DM
      {:id id
       :name (wd/get-element-attr ctx [id "span[data-member-id]"] :data-name)
       :timestamp timestamp
       :type :dm}
      :else ; ad hoc channel
      {:id id
       :name (ad-hoc-channel-name ctx id)
       :timestamp timestamp
       :type :channel}
      )))

(defn list-dms [ctx iframe]
  (println "Getting chats in iframe" iframe)
  (wd/switch-frame* ctx (wd/el->ref iframe))
  (try+
    (println "switched to frame")
    (->> (wd/query-all ctx "span[role=listitem]")
         (map (partial el->id ctx))
         (map (partial id->UserChannel ctx))
         doall)
    (finally
      (println "switched out")
      (wd/switch-frame-top ctx))))

;   (let [chat-buttons (query-all ctx "span[role=listitem]")]
;     )
;   (let [chats (map (partial el->Channel ctx iframe)
;                 (query-all ctx "span[role=listitem]"))]
;     (println "got channels:" chats)
;     (switch-frame-top ctx)
;     chats))

(defn list-channels [ctx iframe] [])

(defn list-chats [ctx]
  (let [[dm-iframe channel-iframe] (wd/query-all ctx "div[role=navigation] iframe")]
    (concat
      (list-dms ctx dm-iframe)
      (list-channels ctx channel-iframe))))
  ; (as-> (query-all ctx "div[role=navigation] iframe") $
  ;       (mapcat (partial list-chats-in-iframe ctx) $)
  ;       (map (partial chat-info ctx) $)))

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

(defn -main
  [& argv]
  (let [opts (parse-opts argv)
        ctx (create-browser opts)]
    (println "OPTS" opts)
    (try+
      (init-browser! ctx)
      (list-chats ctx)
      (finally (wd/quit ctx)))))
  ; (let []
  ;   (with-chrome [ctx {;:profile (opts :profile)
  ;     (println "OPTIONS" opts)
  ;     (println "WEBDRIVER" ctx)
  ;     (init-browser! ctx)
  ;     (println (list-chats ctx)))))
