(ns hangbrain.util
  "Miscellaneous shared functions."
  (:refer-clojure :exclude [def defn defmethod defrecord fn letfn])
  (:require
    [etaoin.api :as wd]
    [io.aviso.repl]
    [io.aviso.logging]
    [schema.core :as s :refer [def defn defmethod defrecord defschema fn letfn]]
    [clojure.string :as string])
  (:import
    [java.time Instant]))

(defn millis->datetime
  [ms]
  (.toString (Instant/ofEpochMilli (Long/parseLong ms))))

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

(defn ->IRCNick
  [name]
  (-> name
      string/trim
      (string/replace #"[ !@:]+", "-")))
