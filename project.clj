(defproject parser "1.0.0-SNAPSHOT"
  :url "http://github.com/ghadishayban/doublehockeysticks"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :description "FIXME: write description"
  :main ^:skip-aot parser.parse
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {:dev {:dependencies [[criterium "0.3.0-SNAPSHOT"]]}})
