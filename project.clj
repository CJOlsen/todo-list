(defproject todo "0.1.0-SNAPSHOT"
  :description "just a super basic todo list"
  :url "none"
  :license {:name "GNU GPLv3"
            :url "http://www.gnu.org"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [seesaw "1.4.3"]]
  :main todo.core
  :profiles {:uberjar {:aot :all}}
  :java-source-paths ["src"])
