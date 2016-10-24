{:user
  {:dependencies
     [[org.clojure/tools.namespace "0.2.11"]
      [org.clojure/tools.nrepl "0.2.12"]
      [pjstadig/humane-test-output "0.8.1"]]
   :plugins
     [[lein-ancient "0.6.10"]
      [lein-pprint "1.1.2"]
      [com.jakemccrary/lein-test-refresh "0.17.0"]
      [lein-kibit "0.1.2"]
      [jonase/eastwood "0.2.3"]
      [lein-bikeshed "0.3.0"]
      [lein-cljfmt "0.5.6"]
      [lein-exec "0.3.6"]
      [lein-clean-m2 "0.1.2"]
      [lein-ns-dep-graph "0.1.0-SNAPSHOT"]]
   :injections
    [(require 'pjstadig.humane-test-output)
     (pjstadig.humane-test-output/activate!)
     (require '[clojure.tools.namespace.repl :refer [refresh]])]}}
