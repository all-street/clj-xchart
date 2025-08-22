(defproject com.hypirion/clj-xchart "0.3.1-SNAPSHOT"
  :description "XChart wrapper for Clojure"
  :url "https://github.com/hyPiRion/clj-xchart"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [de.erichseifert.vectorgraphics2d/VectorGraphics2D "0.13"]
                 [org.knowm.xchart/xchart "3.8.8"]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:unchecked"]
  :plugins [[lein-codox "0.10.8"]]
  :deploy-repositories [["releases" :clojars]]
  :codox {:source-uri "https://github.com/foo/bar/blob/{version}/{filepath}#L{line}"}
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}})
