(defproject interprete-racket "0.1.0-SNAPSHOT"
  :description "Interprete de Racket"
  :url "http://github.com/FacuMastri/racket-interpreter"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :repl-options {:init-ns interprete-racket.core}
  :main interprete-racket.core)