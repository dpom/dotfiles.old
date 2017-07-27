{:user {:plugins [[lein-ancient "0.6.10" :exclusions [org.clojure/clojure]]
                  [lein-codeindex "0.1.0"]
                  ;; [lein-checkall "0.1.1" :exclusions [org.clojure/tools.namespace org.clojure/clojure]]
                  ;; [venantius/ultra "0.5.1"]
                  ]
        }
 :repl {:plugins [[cider/cider-nrepl "0.15.0"]
                   [refactor-nrepl "2.3.1"]]
         :dependencies [[alembic "0.3.2"]
                        [org.clojure/tools.nrepl "0.2.13"]]
         }
 }
