(defproject allstreet/clj-xchart "0.0.0"
  :description "XChart wrapper for Clojure"
  :url "https://github.com/all-street/clj-xchart/"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.12.1"]
                 [de.erichseifert.vectorgraphics2d/VectorGraphics2D "0.13"]
                 [org.knowm.xchart/xchart "3.8.8"]]
  :source-paths ["src/clj"]
  :java-source-paths ["src/java"]
  :javac-options ["-Xlint:unchecked"]
  :deploy-repositories [["releases" :clojars]]
  :codox {:source-uri "https://github.com/foo/bar/blob/{version}/{filepath}#L{line}"}
  :profiles {:dev {:dependencies [[org.clojure/test.check "1.1.1"]]}}

  :plugins [[lein-codox "0.10.8"]
            [lein-modules "0.3.11"]
            [lein-cloverage "1.2.2"]]

  :cloverage {:output "private/cloverage"}

  :aliases {"testing" ["do" ["check"] ["cloverage"] ["install"]]
            "build" ["install"]})
