(defproject cs441 "0.0.1-SNAPSHOT"
  :description "Clojure app to sort a given large list of numbers using a parallelized merge sort algorithm."
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/core.async "0.4.490"]]
  :aot [cs441.core]
  :main cs441.core)
