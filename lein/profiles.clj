{:user
  {:dependencies
     [[org.clojure/tools.namespace "0.2.11"]
      [org.clojure/tools.nrepl "0.2.13"]
      [im.chit/lucid.mind "1.3.10"]
      [im.chit/lucid.package "1.3.10"]
      [im.chit/lucid.unit "1.3.10"]
      [im.chit/lucid.core.debug "1.3.10"]
      [im.chit/lucid.core.inject "1.3.10"]
      [spyscope "0.1.6"]
      [proto-repl "0.3.1"]]
   :plugins
     [[venantius/ultra "0.5.1"]
      [lein-ancient "0.6.10"]
      [lein-pprint "1.1.2"]
      [com.jakemccrary/lein-test-refresh "0.20.0"]
      [lein-kibit "0.1.5"]
      [jonase/eastwood "0.2.4"]
      [lein-bikeshed "0.4.1"]
      [lein-cljfmt "0.5.6"]
      [lein-exec "0.3.6"]
      [lein-ns-dep-graph "0.2.0-SNAPSHOT"]]
   :injections
     [(require 'spyscope.core)
      (require 'clojure.tools.namespace.repl
               'clojure.repl
               'clojure.pprint
               'proto-repl.saved-values
               'spyscope.core
               '[lucid.core.inject :as inject])
      (inject/in [lucid.package pull]
                 [lucid.unit import scaffold purge missing orphaned in-order? arrange]
                 [lucid.mind .& .> .? .* .% .%> .>var .>ns]
                 [lucid.core.debug :refer [[dbg-> *->] [dbg->> *->>]]]
                 clojure.core [clojure.pprint pprint]
                 clojure.core [clojure.repl dir doc pst source])]

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
