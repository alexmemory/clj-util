(defproject org.clojars.alexmemory/clj-util "0.1.1-SNAPSHOT"
  :description "Utilities for Clojure"
  :url "https://github.com/alexmemory/clj-util"
  :dependencies [
                 [clj-time "0.11.0"]
                 [org.clojure/clojure "1.8.0"]
                 [org.clojure/core.async "0.2.395"] 
                 ]
  :aot :all
  :pedantic :warn                       ; Class path warnings
  )
