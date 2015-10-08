(defproject mami "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :main ^:skip-aot mami.core
  :profiles {:uberjar {:aot :all}}
  :plugins [[s3-wagon-private "1.1.2"]
            [rplevy/lein-deploy-app "0.2.1"]]
  :deploy-app {:s3-bucket "s3p://opsee-releases/clj" :creds :env}
  :repositories [["snapshots" {:url "s3p://opsee-maven-snapshots/snapshot"
                               :username :env
                               :passphrase :env}]]
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [amazonica "0.3.21"]
                 [cheshire "5.4.0"]
                 [clj-time "0.9.0"]
                 [clj-ssh "0.5.11"]
                 [org.slf4j/slf4j-log4j12 "1.7.7"]
                 [log4j/log4j "1.2.17" :exclusions [javax.mail/mail
                                                    javax.jms/jms
                                                    com.sun.jmdk/jmxtools
                                                    com.sun.jmx/jmxri]]
                 [de.ubercode.clostache/clostache "1.4.0"]
                 [org.clojure/tools.cli "0.3.1"]])
