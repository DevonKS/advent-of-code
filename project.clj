(defproject advent-of-code-2020 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.combinatorics "0.1.6"]]
  :target-path "target/%s"
  :profiles {:uberjar {:main ^:skip-aot advent-of-code-2020.core
                       :aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}
             :dev {:dependencies [[cljfmt "0.7.0"]
                                  [org.clojure/tools.namespace "1.0.0"]]
                   :plugins [[cider/cider-nrepl "0.25.4"]
                             [refactor-nrepl "2.5.0"]]}})
