(defproject discwar-ai "0.0.1"
  :description "Dennis' Discwar AIs"
  :main discwar.ai
  :dependencies
    [[org.clojure/clojure "1.2.1"]
     [org.clojure/clojure-contrib "1.2.0"]
     [org.clojure/tools.logging "0.2.3"]
     [ch.qos.logback/logback-classic "1.0.0"]
     [ring/ring-jetty-adapter "0.2.5"]
     [ring-json-params "0.1.0"]
     [compojure "0.4.0"]
     [clj-json "0.2.0"]]
  :dev-dependencies
    [[lein-run "1.0.0-SNAPSHOT"]
     [ring/ring-devel "0.2.5"]])
