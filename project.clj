(defproject sudoku "0.0.1-SNAPSHOT"
  :description "Sudoku app using clojure script"
  :dependencies [[org.clojure/core.logic "0.7.5"]]
		:plugins [[lein-cljsbuild "0.3.0"]]
  :cljsbuild {
              :builds {:main { :source-paths ["src"] } }
              })