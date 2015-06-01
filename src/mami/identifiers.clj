(ns mami.identifiers
  (:import [java.math BigInteger]
           [java.security SecureRandom]
           [java.lang StringBuilder]))

(def digits "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")
(def regexp #"^[0-9A-Za-z]+$")
(def base (BigInteger/valueOf 62))

(def random (new SecureRandom))

(defn base62-encode [big-int]
  (if (== -1 (.compareTo big-int BigInteger/ZERO))
    (throw (new IllegalArgumentException "number must not be negative"))
    (let [result (new StringBuilder)]
      (loop [number big-int]
        (let [[new-number digit] (.divideAndRemainder number base)]
          (->> digit
               (.intValue)
               (.charAt digits)
               (.insert result 0))
          (if (== 1 (.compareTo new-number BigInteger/ZERO))
            (recur new-number)
            (if (== 0 (.length result))
              (.substring digits 0 1)
              (.toString result))))))))

(defn generate []
  (base62-encode (new BigInteger 128 random)))