(defproject hangbrain "0.1.0-SNAPSHOT"
  :description "A Google Chat <-> IRC proxy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars {*warn-on-reflection* false
                *assert* true}
  :dependencies [[com.taoensso/timbre "5.1.2"]
                 [etaoin "0.4.6"]
                 [expound "0.8.4"]
                 [io.aviso/pretty "1.1"]
                 [org.clojure/clojure "1.10.0"]
                 [prismatic/schema "1.1.12"]
                 [ca.ancilla/zeiat "0.1.0"]]
  :main ^:skip-aot hangbrain.core
  :target-path "target/%s"
  :middleware [io.aviso.lein-pretty/inject]
  :jvm-opts
  ["-Dclojure.tools.logging.factory=clojure.tools.logging.impl/jul-factory"
   "-Djava.util.logging.ConsoleHandler.level=ALL"]
  :plugins [
            [io.aviso/pretty "1.1"]
            ]
  :profiles {:uberjar {:aot :all}})
