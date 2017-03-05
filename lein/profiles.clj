{:user
  {:dependencies
     [[org.clojure/tools.namespace "0.2.11"]
      [org.clojure/tools.nrepl "0.2.12"]
      ;[pjstadig/humane-test-output "0.8.1"]
      [proto-repl "0.3.1"]]
   :plugins
     [[venantius/ultra "0.5.1"]
      [lein-ancient "0.6.10"]
      [lein-pprint "1.1.2"]
      [com.jakemccrary/lein-test-refresh "0.18.1"]
      ; [lein-kibit "0.1.2"]
      ; [jonase/eastwood "0.2.3"]
      ; [lein-bikeshed "0.4.0"]
      [lein-cljfmt "0.5.6"]
      [lein-exec "0.3.6"]
      [lein-ns-dep-graph "0.1.0-SNAPSHOT"]]
   :injections
;    [(require 'pjstadig.humane-test-output)
;     (pjstadig.humane-test-output/activate!)
     [(require '[clojure.tools.namespace.repl :refer [refresh]])]
   :ultra
    {:color-scheme
      {:delimiter [:white]
       :tag       [:red]

       :nil       [:cyan]
       :boolean   [:cyan]
       :number    [:cyan]
       :string    [:green]
       :character [:cyan]
       :keyword   [:yellow]
       :symbol    nil

       :function-symbol [:magenta]
       :class-delimiter [:magenta]
       :class-name nil

       :exception [:red]}}}}
