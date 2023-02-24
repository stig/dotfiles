{:user {:pedantic? :warn}
 :deps-plus {:plugins [[com.circleci/deps-plus "0.1.0-SNAPSHOT"]]}
 :repl {:dependencies [[nrepl/nrepl "RELEASE"]]
        :plugins [[refactor-nrepl/refactor-nrepl "RELEASE"]
                  [cider/cider-nrepl "RELEASE"]
                  [mx.cider/enrich-classpath "RELEASE"]]}}
