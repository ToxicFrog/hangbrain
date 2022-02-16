(defproject hangbrain "0.1.0-SNAPSHOT"
  :description "A Google Chat <-> IRC proxy"
  :url "http://github.com/toxicfrog/hangbrain/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :global-vars {*warn-on-reflection* false
                *assert* true}
  :dependencies [[clj-commons/fs "1.6.307"]
                 [com.taoensso/timbre "5.1.2"]
                 [dev.dirs/directories "26"]
                 [etaoin "0.4.6"]
                 [expound "0.8.4"]
                 [io.aviso/pretty "1.1"]
                 [org.apache.commons/commons-text "1.9"]
                 [org.clojure/clojure "1.10.0"]
                 [prismatic/schema "1.1.12"]
                 [ca.ancilla/zeiat "0.2.0-SNAPSHOT"]]
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
