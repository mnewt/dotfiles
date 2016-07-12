{:user
  {:dependencies
     [[org.clojure/tools.namespace "LATEST"]
      [pjstadig/humane-test-output "LATEST"]]
   :plugins
     [[lein-ancient "LATEST"]
      [lein-pprint "LATEST"]
      ; [venantius/ultra "LATEST"]
      [com.jakemccrary/lein-test-refresh "LATEST"]
      [lein-kibit "LATEST"]
      [jonase/eastwood "LATEST"]
      [lein-bikeshed "LATEST"]
      [lein-cljfmt "LATEST"]
      [lein-exec "LATEST"]
      [lein-bin "LATEST"]]
   :injections
    [(require 'pjstadig.humane-test-output)
     (pjstadig.humane-test-output/activate!)
     (require '[clojure.tools.namespace.repl :refer [refresh]])]}}
 ; :test-refresh
 ;  {:plugins
 ;    [[venantius/ultra "LATEST"]]}}
