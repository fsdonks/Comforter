(defproject comforter "0.0.1-SNAPSHOT"
  :description "Post processor for requirements analysis."
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [marathon-schemas "4.1.6-SNAPSHOT"
                  :exclusions [spork]]
                 [spork "0.2.1.1-SNAPSHOT"]]
  :aot [comforter.main]
  :main comforter.main)
