{:user {:plugins [[lein-licenses "0.2.0"]
                  [lein-ancient "0.6.15"]
                  [jonase/eastwood "0.2.6"]
                  [lein-kibit "0.1.6"]
                  [codox "0.7.2"]
                  [lein-test-out "0.3.1"]
                  [lein-cloverage "1.0.6"]
                  [lein-release "1.0.5"]
                  [test2junit "1.2.1"]]
        ;; change snapshot update policy so that bamboo always downloads snapshots from Nexus
        :update :always 
        :dependencies [[pjstadig/humane-test-output "0.7.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        ;; By default lein will run a clean before deploying to prevent
        ;; undeclared AOT from leaking to downstream consumers; this disables
        ;; that behaviour.
         :auto-clean false}}
