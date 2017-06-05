(defproject recipes "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]
                 [semantic-csv "0.2.1-alpha1"]
                 [hiccup "2.0.0-alpha1"]]
  :main ^:skip-aot recipes.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
