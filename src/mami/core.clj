(ns mami.core
  (:gen-class)
  (:require [amazonica.aws.ec2 :refer :all]
            [amazonica.aws.cloudformation :as cf]
            [clojure.java.shell :refer [sh]]
            [cheshire.core :refer :all]
            [clostache.parser :refer :all]
            [clj-time.core :as t]
            [mami.identifiers :as identifiers]
            [clj-time.format :as f]
            [clojure.tools.cli :as cli]
            [clj-ssh.ssh :refer :all]
            [clj-http.client :as client]
            [clojure.string :as str]
            [clojure.tools.logging :as log]
            [clojure.pprint :refer [pprint]]
            [clojure.string :refer [trim-newline]]
            [mami.regions :refer :all])
  (:import (java.io File)
           (java.nio.file Files Path FileSystems)
           (java.nio.file.attribute PosixFilePermission)))

(defn git-rev []
  (trim-newline (:out (sh "git" "rev-parse" "HEAD"))))

(defn timestamp []
  (f/unparse (f/formatters :date-hour-minute-second) (t/now)))

(defn get-public-ip [creds instance-id]
  (let [{[{[instance] :instances}] :reservations} (describe-instances creds {:instance-ids [instance-id]})]
    (log/info "instance is now started")
    (:public-ip-address instance)))

(defn wait-for-state [creds state instance-id]
  (loop [{[{[{status :state}] :instances}] :reservations} (describe-instances creds {:instance-ids [instance-id]})]
    (log/info "instance status" status)
    (if-not (= state (:name status))
      (do
        (Thread/sleep 1000)
        (recur (describe-instances creds {:instance-ids [instance-id]}))))))

(defn do-with-ssh [keypair username public-ip & fns]
  (let [agent (ssh-agent {})]
    (add-identity agent {:private-key (:key-material keypair)})
    (let [sesh (atom nil)]
      (loop [connect-failed (atom false)]
        (try
          (reset! sesh (session agent public-ip {:username username :strict-host-key-checking :no}))
          (connect @sesh 15000)
          (catch Exception ex
            (log/info "connect attempt failed, will retry" ex)
            (Thread/sleep 5000)
            (reset! connect-failed true)))
        (if @connect-failed (recur (atom false))))
      (with-connection @sesh
                       (doseq [fn fns] (fn @sesh))))))

(defn run-instructions [instructions]
  (fn [session]
    (let [result (ssh session {:in (str/join ";" instructions)})]
      (println (str/replace (:out result) #"([\s]?[\r]|[\x1B]\[(H|J))" ""))
      (when-not (= 0 (:exit result))
        (throw (Exception. "ssh instructions failed."))))))

(defn create-staging-dir [keypair username public-ip staging]
  (do-with-ssh keypair username public-ip
               (run-instructions [(str "sudo mkdir -p " staging)
                                  (str "sudo chown " username " " staging)])))

(defn upload-paths [paths to-dir & {:keys [recursive]
                                    :or {recursive false}}]
  (fn [session]
    (scp-to session paths to-dir :recursive recursive)))

(defn cleanup [creds instance-details]
  (log/info "Cleaning up after ourselves.")
  (terminate-instances creds {:instance-ids [(:instance-id instance-details)]})
  (wait-for-state creds "terminated" (:instance-id instance-details))
  (delete-key-pair creds {:key-name (:key-name (:key-pair instance-details))})
  (delete-security-group creds {:group-id (:sg-id instance-details)}))

(defn print-debug [creds keypair username public-ip]
  (let [key (:key-material keypair)
        filename (str public-ip ".pem")]
    (spit filename key)
    (Files/setPosixFilePermissions
      (.getPath (FileSystems/getDefault) "." (into-array String [filename]))
      #{PosixFilePermission/OWNER_READ PosixFilePermission/OWNER_WRITE})
    (log/info "Build failed, instance preserved for debug. To login run:")
    (log/info (str "ssh -i " filename " " username "@" public-ip))))

;;;; these get invoked by the corresponding "type" in the prepare steps

(defn shell [keypair username public-ip _ shell-config]
  (let [instructions (:instructions shell-config)]
    (do-with-ssh keypair username public-ip
                 (run-instructions instructions))))

(defn systemd-unit-type [unit-name]
  (last (.split unit-name "\\.")))

(defn systemd-instructions [unit-type unit-name staging]
    (let [instructions [(str "sudo mv " staging "/" unit-name " /etc/systemd/system/")]]
      (if (= unit-type "service")
            (into [] (concat instructions [(str "sudo systemctl enable /etc/systemd/system/" unit-name)
                                             (str "sudo systemctl start " unit-name)
                                             (str "sudo systemctl status " unit-name)])) instructions)))

(defn systemd [keypair username public-ip staging systemd-config]
  (let [unit-file (:unit-file systemd-config)
        unit-name (.getName (File. unit-file))
        unit-type (systemd-unit-type unit-name)]
    (do-with-ssh keypair username public-ip
                 (upload-paths unit-file staging)
                 (run-instructions (systemd-instructions unit-type unit-name staging)))))

(defn waitfor [keypair username public-ip _ waitfor-config]
  (let [test-script (:test waitfor-config)]
    (do-with-ssh keypair username public-ip
      (fn [session]
        (loop [count 0]
          (if (> count 30)
            (throw (Exception. "Waitfor timed out."))
            (let [output (ssh session {:in (str "test " test-script)})]
              (println (str/replace (:out output) #"([\s]?[\r]|[\x1B]\[(H|J))" ""))
              (when-not (= 0 (:exit output))
                (Thread/sleep 2000)
                (recur (inc count))))))))))

(defn chef-solo [keypair username public-ip staging chef-config]
  (let [cookbook_paths (:cookbook_paths chef-config)
        run_list (:run_list chef-config)]
    (do-with-ssh keypair username public-ip
                 (run-instructions ["mkdir -p staging"
                                    "curl -L https://www.opscode.com/chef/install.sh | sudo bash"
                                    (str "echo 'cookbook_path ["
                                         (str/join "," (map #(str "\"" % "\"") cookbook_paths))
                                         "]\n' > staging/solo.rb")
                                    (str "echo '"
                                         (generate-string {:run_list run_list})
                                         "\n' > staging/node.json")])
                 (upload-paths cookbook_paths staging :recursive true)
                 (run-instructions ["cd staging"
                                    "chef-solo -c solo.rb -j node.json"]))))

(defn scp [keypair username public-ip staging scp-config]
  (let [from-dir (:from scp-config)]
    (do-with-ssh keypair username public-ip
                 (upload-paths from-dir staging))))

(defn cleanup-prepare [keypair username public-ip staging]
  (do-with-ssh keypair username public-ip
               (run-instructions [(str "rm -rf " staging)])))


(defn get-latest-coreos-ami-all
  "returns a map of the latest coreos amis for all regions"
 ([url]
    (try
      (parse-string ((client/get url {:throw-exceptions true}) :body))
    (catch Exception e (log/error (str "caught: Error retrieving most recent COREOS AMI -- " (.getMessage e)))))))

(defn get-latest-coreos-ami
  "returns the ami-image id for the most recent coreos-ami for a given region and type"
  ([region type]
   (((get-latest-coreos-ami-all "https://coreos.com/dist/aws/aws-stable.json") region) type)))

(defn get-build-ami
  "returns the id of the ami from either the most recent or from config file if source-ami is specified"
  ([region type config]
   (if (contains? config :source-ami) (config :source-ami) (get-latest-coreos-ami region type))))


;;; instance launching
;;; XXX Modify this for newest AMI
(defn launch-instance [creds config]
  (log/info "launching ec2 instance")
  (let [id (identifiers/generate)
        keypair-name (str "keypair-" id)
        {keypair :key-pair} (create-key-pair creds {:key-name keypair-name})
        {sg-id :group-id} (create-security-group creds {:group-name (str "mami-sg-" id) :description "mami temporary sg"})]
    (log/info "launching with key:\n" (:key-material keypair))
    (authorize-security-group-ingress creds {:group-id sg-id :cidr-ip "0.0.0.0/0" :from-port 22 :to-port 22 :ip-protocol "tcp"})
    (let [{{[{instance-id :instance-id}] :instances} :reservation} (run-instances creds {:security-group-ids [sg-id]
                                                           :image-id (get-build-ami (config :build-region) "hvm" config)
                                                           :iam-instance-profile {:arn "arn:aws:iam::933693344490:instance-profile/BastionRole"}
                                                           :min-count 1
                                                           :max-count 1
                                                           :instance-type (:instance-type config)
                                                           :key-name keypair-name})
          public-ip (do
                      ;; Race condition between run-instances and describe-instances in wait-for-state
                      ;; So just sleep for a second before we try to wait-for-state. Should cut down
                      ;; on the number of times that we have to clean up after an exception.
                      (Thread/sleep 1000)
                      (wait-for-state creds "running" instance-id)
                      (get-public-ip creds instance-id))]
      (log/info "instance" instance-id "successfully launched reachable at" public-ip)
      {:key-pair keypair
       :instance-id instance-id
       :sg-id sg-id
       :public-ip public-ip})))

(defn reboot-and-wait [creds instance-details]
  (let [instance-id (:instance-id instance-details)]
    (log/info "rebooting" instance-id)
    (reboot-instances creds {:instance-ids [instance-id]})
    (wait-for-state creds "running" instance-id)))

(defn tag-image [creds image-id tags]
    (log/info "tagging image" image-id "with" tags)
    (create-tags creds {:resources [image-id]
                        :tags      tags}))

(defn make-ebs-image [creds instance-details config]
  (let [{image-id :image-id} (create-image creds {:instance-id (:instance-id instance-details)
                                                  :name (:ami-name config)
                                                  :description (:ami-description config)
                                                  :no-reboot true})]
    (log/info "create image id" image-id)
    image-id))

(defn copy-to [from-region image-id config]
  (let [to-regions (:copy-to config)]
    (apply hash-map
           (flatten
             (for [to-region to-regions
                   :let [ep {:endpoint (name to-region)}]]

                 (let [{dst-image-id :image-id} (copy-image ep {:name (:ami-name config)
                                                                :description (:ami-description config)
                                                                :source-image-id image-id
                                                                :source-region from-region})
                       tags (concat (:ami-tags config)
                              [["sha" (git-rev)]
                               ["release" (:release config)]])]
                   (log/info "copied to" to-region "with id" dst-image-id)
                   (tag-image ep dst-image-id tags)
                   [to-region dst-image-id]))))))

(defn parse-tags [arg]
  (map
    (fn [tagsec]
      (let [[name vals] (str/split tagsec #"\s*=\s*")]
        {:name (str "tag:" name)
         :values (when vals (str/split vals #"\s*,\s*"))}))
    (str/split arg #"\s*;\s*")))

(def common-options
  [["-g" "--regions REGIONS" "a comma separated list of regions to do the upload (defaults to all)"
    :default "all"
    :parse-fn #(str/split % #",\s*")]
   ["-f" "--filter TAGS" "a semicolon separated list of tag filters name=value1,value2"
    :default ""
    :parse-fn parse-tags]
   ["-r" "--release TAG" "Tag the release (build [default], beta, stable)"
    :default "build"]])

(def build-options
  (concat common-options
    [[nil "--no-cleanup" "Disable cleanup (default false)"
      :default false]
     [nil "--bastion-version TAG" "Test against a particular tag of the bastion container image"
      :default "latest"]
     ["-b" "--build-ami" "Build an AMI instead from the created instance"
      :default false]]))

(defn run-build [args config]
  (let [{:keys [options _ _ _]} (cli/parse-opts args build-options)
        config (merge config options)
        error (atom false)
        creds {:endpoint (:build-region config)}
        instance-details (launch-instance creds config)
        instance-id (:instance-id instance-details)
        keypair (:key-pair instance-details)
        public-ip (:public-ip instance-details)
        preparation-steps (:prepare config)
        username (:ssh-username config)
        staging-dir (:staging config)]
    (try
      (create-staging-dir keypair username public-ip staging-dir)
      ; I don't really know of a way to do this that isn't quite so opaque to the mami user,
      ; but I don't really see the value in making this super flexible right now. -G
      (let [customer-id (System/getenv "CUSTOMER_ID")
            bastion-id (System/getenv "BASTION_ID")
            vpn-password (System/getenv "VPN_PASSWORD")
            bastion-ingress (System/getenv "ENABLE_BASTION_INGRESS")
            bastion-version (:bastion-version config)
            env-contents (str
                           "CUSTOMER_ID=" customer-id "\n"
                           "BASTION_ID=" bastion-id "\n"
                           "VPN_PASSWORD=" vpn-password "\n"
                           "BASTION_VERSION=" bastion-version "\n"
                           "ENABLE_BASTION_INGRESS=" bastion-ingress "\n"
                           )]
        (spit "bastion-env.sh" env-contents)
        (scp keypair username public-ip staging-dir {:from "bastion-env.sh"})
        (shell keypair username public-ip nil {:instructions [
                                                              "sudo mkdir /etc/opsee"
                                                              (str "sudo mv " staging-dir "/bastion-env.sh /etc/opsee/bastion-env.sh")]}))
      (doseq [step preparation-steps
              :let [type (:type step)]]
        ((eval (symbol "mami.core" type)) keypair username public-ip staging-dir step))
      (cleanup-prepare keypair username public-ip staging-dir)
      (if (:reboot-before-build config)
        (reboot-and-wait creds instance-details))
      (stop-instances creds {:instance-ids [instance-id]})
      (wait-for-state creds "stopped" instance-id)
      (if (:build-ami config)
        (let [image-id (make-ebs-image creds instance-details config)
              image-ids (copy-to (:build-region config) image-id config)]
          (doseq [[] (seq image-ids)])))
      (catch Exception ex
        (log/error ex "Got exception during build")
        (reset! error true)))
    (if (or @error (:no-cleanup options))
      (do
        (if (:cleanup-on-error config)
          (cleanup creds instance-details)
          (print-debug creds keypair username public-ip))
        (System/exit 1))
      (do
        (cleanup creds instance-details)
        (System/exit 0)))))

(defn get-regions [region-option]
  (if (= "all" region-option)
    all-regions
    (map #(keyword %) region-option)))

(def tag-options
  (concat common-options
    [["-s" "--sha" "sha tag of the image to set the release tag (default: current git revhash"
      :default nil]]))

(defn run-tag [args config])

(def latest-options
  (concat common-options
     [["-o" "--owner OWNER" "the owning acct id for the AMIs" :default "933693344490"]]))

(defn run-latest [args config]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args latest-options)
        filters (flatten [(if-let [sha (:sha options)]
                            {:name "tag:sha" :values [sha]}
                            [])
                          (if-let [release (:release options)]
                            {:name "tag:release" :values [release]}
                            [])])]
    (doseq [region (get-regions (:regions options))
            :let [creds {:endpoint (name region)}
                  {images :images} (describe-images creds :owners [(:owner options)] :filters filters)
                  image (first
                          (sort-by :name #(compare %2 %1) images))]]
      (pprint {region image}))))

(def clear-options
  (concat common-options
    [[]]))

(defn run-clear-amis [args config]
  )

(defn run-clear-stacks [args config]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args clear-options)]
    (doseq [region (get-regions (:regions options))
            :let [creds {:endpoint (name region)}]]
      (doseq [stack (:stacks (cf/describe-stacks creds))]
        (cf/delete-stack creds stack)))))

(defn normalize-config [config]
  (merge config
    {
     :copy-to (let [copy-to (:copy-to config)]
                (cond
                  (seq? copy-to) copy-to
                  (= "all" copy-to) (all-regions-except (:build-region config))
                  :else [copy-to]))
     }))

(defn -main [& args]
  (let [action (first args)
        options (drop 1 (drop-last 1 args))
        config-file (last args)
        config-template (slurp config-file)
        env {:git-rev (git-rev)
             :timestamp (timestamp)
             :clean-timestamp (str/replace (timestamp) #":" ".")}
        config (normalize-config (parse-string (render config-template env) true))]
    (case action
      "build" (run-build options config)
      "tag" (run-tag options config)
      "latest" (run-latest options config)
      "clear-amis" (run-clear-amis options config)
      "clear-stacks" (run-clear-stacks options config))))
