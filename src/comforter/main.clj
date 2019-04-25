;;Shim class for running comforter without
;;aot compilation issues.
;;entrypoint for comforter gui.
(ns comforter.main
  (:gen-class :main true))

;;This is the main entry point for comforter.
;;It's a good example of a shim-class, and
;;requires some arcane features to get things
;;working, since we're creating repls on other
;;threads.
(defn -main [& args]
  ;;clojure.set isn't imported by default, causing errors when
  ;;aot-compiling in some places.
  (require 'clojure.set)
  (require  'clojure.java.io)
  (if (seq args)
    (clojure.main/repl)
    (binding [*ns* *ns*]
      (require 'comforter.core)
      (in-ns 'comforter.core)
      ;;if we don't use resolve, then we get compile-time aot
      ;;dependency on marathon.core.  This allows us to shim the
      ;;class.
      
      (apply (resolve 'comforter.core/gui) args))))
