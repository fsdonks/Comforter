(defproject comforter "0.0.1-SNAPSHOT"
  :description "Post processor for requirements analysis."
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [spork "0.2.1.1-SNAPSHOT"]]
  :aot [comforter.core]
  :main comforter.core)
