(defproject hangbrain "0.1.0-SNAPSHOT"
  :description "A Google Chat <-> IRC proxy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [prismatic/schema "1.1.12"]
                 [etaoin "0.4.6"]]
  :main ^:skip-aot hangbrain.core
  :target-path "target/%s"
  :middleware [io.aviso.lein-pretty/inject]
  :plugins [
    [io.aviso/pretty "1.1"]
  ]
  :profiles {:uberjar {:aot :all}})
