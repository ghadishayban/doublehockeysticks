(defproject parser "1.0.0-SNAPSHOT"
  :url "http://github.com/ghadishayban/doublehockeysticks"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :description "FIXME: write description"
  :main ^:skip-aot parser.structure-analysis
  :jvm-opts ["-Xmx4096m"]
  :dependencies [[org.clojure/clojure "1.5.0-master-SNAPSHOT"]
                 [clojure-csv "2.0.0-alpha2"]
                 [org.clojure/core.cache "0.6.3-SNAPSHOT"]]
  :repositories [["sonatype" {:snapshots true :url "https://oss.sonatype.org/content/groups/public/"}]]
  :profiles {:dev {:dependencies [[criterium "0.3.0-SNAPSHOT"]]}})
