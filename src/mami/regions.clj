(ns mami.regions)

(def regions #{:ap-northeast-1 :ap-southeast-1 :ap-southeast-2
               :eu-central-1 :eu-west-1
               :sa-east-1
               :us-east-1 :us-west-1 :us-west-2})

(defn all-regions-except [this-region]
  (disj regions (keyword this-region)))

