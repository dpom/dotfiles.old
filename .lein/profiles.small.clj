{:user
 {:plugins [[lein-ancient "0.6.10" :exclusions [org.clojure/clojure]]
            [lein-codeindex "0.1.0"]
            [lein-checkall "0.1.1" :exclusions [org.clojure/tools.namespace org.clojure/clojure]]
            [com.jakemccrary/lein-test-refresh "0.16.0"]]
                   :dependencies [[pjstadig/humane-test-output "0.8.0"]
                 ]
  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)]
  :test-refresh {:quiet true
                 ;; :notify-command ["terminal-notifier" "-title" "Tests" "-message"]
                 :growl true
                 :changes-only true}
  }}
 
