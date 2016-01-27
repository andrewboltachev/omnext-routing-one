(defproject omnext-routing-one "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ^:replace ["-Xmx1g" "-server"]

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [figwheel-sidecar "0.5.0-3" :scope "test"]
                 [org.clojure/core.async "0.2.374"]
                 [org.omcljs/om "1.0.0-alpha29-SNAPSHOT"]
                 [ring "1.4.0"]
                 [compojure "1.4.0"]
                 [figwheel "0.5.0-3"]
                 [fogus/ring-edn "0.3.0"]
                 [com.datomic/datomic-free "0.9.5344" :exclusions [joda-time]]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-figwheel "0.5.0-3"]]


  :source-paths ["src/clj" "src/cljs"]
  :resource-paths ["resources"]
  :clean-targets ^{:protect false} ["resources/public/js/out"
                                    "resources/public/js/main.js"]

  :figwheel {:ring-handler omnext-routing-one.core/handler}

  :cljsbuild {:builds [{:id "dev"
                        :source-paths ["src/clj" "src/cljs"]
                        :figwheel true
                        :compiler {:output-to "resources/public/js/main.js"
                                   :output-dir "resources/public/js/out"
                                   :main omnext-routing-one.core
                                   :asset-path "js/out"
                                   :optimizations :none
                                   :source-map true}}]})
