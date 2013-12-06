(defproject todo "0.1.1"
  :description "just a basic todo list, split into productive/non-productive lists"
  :url "none"
  :license {:name "GNU GPLv3"
            :url "http://www.gnu.org"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.3"]]
  :main todo.core2
  :profiles {:uberjar {:aot :all}}
  :java-source-paths ["src"])
