(defproject flight-simulator "0.1.0-SNAPSHOT"
  :description "a multicore flight ticket reservation simulator"
  :url "https://github.com/spartam/Flight-reservation-system"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]]
  :main ^:skip-aot flight-simulator.sequential
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
