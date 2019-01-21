{:user {:plugins [
                  ;; [venantius/ultra "0.5.1"]
                  ]
        :dependencies [[pjstadig/humane-test-output "0.8.2"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        }
 :jvm-opts ["-Dclojure.server.repl={:port 5555 :accept clojure.core.server/repl}"]
}
