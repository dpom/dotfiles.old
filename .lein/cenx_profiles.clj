{ :user {:plugins [[lein-ancient "0.6.15"]
                   [jonase/eastwood "0.3.4"]
                   [lein-kibit "0.1.6"]
                   ;; [refactor-nrepl "2.0.0"]
                   ;; [cider/cider-nrepl "0.18.0"]
                   [codox "0.10.5"]]
        ;; change snapshot update policy so that bamboo always downloads snapshots from Nexus
        :update :always
        :dependencies [[pjstadig/humane-test-output "0.9.0"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        ;; By default lein will run a clean before deploying to prevent
        ;; undeclared AOT from leaking to downstream consumers; this disables
        ;; that behaviour.
         :auto-clean false}}
