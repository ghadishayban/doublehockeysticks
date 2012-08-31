(defproject parser "1.0.0-SNAPSHOT"
  :url "http://github.com/ghadishayban/doublehockeysticks"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :description "FIXME: write description"
  :main parser.structure-analysis
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/core.cache "0.6.2"]]
  :profiles {:dev {:dependencies [[criterium "0.3.0-SNAPSHOT"]]}})
