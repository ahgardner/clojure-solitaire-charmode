(defproject solitaire "1.0-SNAPSHOT"
  :description "Non-graphical solitaire game in Clojure"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"] [clj-time "0.13.0"]]
  :main ^:skip-aot solitaire.core
  :target-path "target"
  :profiles {:uberjar {:aot :all}})
