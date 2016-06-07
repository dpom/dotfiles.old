{:user
 {:plugins [[lein-ancient "0.6.10" :exclusions [org.clojure/clojure]]
            [lein-checkall "0.1.1" :exclusions [org.clojure/tools.namespace org.clojure/clojure]]]
  :dependencies [[pjstadig/humane-test-output "0.8.0"]
                 [spyscope "0.1.5"]
                  [org.clojure/tools.namespace "0.2.10"]
                 ;; [leiningen #=(leiningen.core.main/leiningen-version)]
                 [leiningen #=(leiningen.core.main/leiningen-version) :exclusions [slingshot
                                                                                   commons-logging
                                                                                   org.codehaus.plexus/plexus-utils
                                                                                   org.apache.maven.wagon/wagon-provider-api]]
                 [org.apache.maven.wagon/wagon-provider-api "2.10"]
                 [commons-logging "1.2"]
                  [io.aviso/pretty "0.1.26"]
                  [alembic "0.3.2"]
                 [org.clojars.gjahad/debug-repl "0.3.3"]
                 [im.chit/vinyasa "0.4.3"]]
  :injections [(require 'pjstadig.humane-test-output)
               (pjstadig.humane-test-output/activate!)
               (require 'spyscope.core)
               (require '[vinyasa.inject :as inject])
               (require 'io.aviso.repl)
               (require 'alex-and-georges.debug-repl)
               (inject/in ;; the default injected namespace is `.`

               ;; note that `:refer, :all and :exclude can be used
               [vinyasa.inject :refer [inject [in inject-in]]]  
               [vinyasa.reimport :refer [reimport]]  
               [vinyasa.lein :exclude [*project*]]  

               ;; imports all functions in vinyasa.pull
               [alembic.still [distill pull]]

               ;; inject into clojure.core
               clojure.core
               [vinyasa.reflection .> .? .* .% .%> .& .>ns .>var]

               ;; inject into clojure.core with prefix
               clojure.core >
               [alex-and-georges.debug-repl debug-repl]
               [clojure.pprint pprint]
               [clojure.java.shell sh])]}}
