(ns script
  (:require [nbb.core :refer [slurp]]
            [promesa.core :as p]
            [clojure.string :as str]))

(def fs (js/require "fs"))
(def input
  (->>
   (fs.readFileSync "./input" "utf8")
   (str/split-lines)))

(defn- parse-condition
  [string]
  (let [matches (re-find #"([0-9]+)-([0-9]+) ([a-z])" string)]
    {:min (nth matches 1)
     :max (nth matches 2)
     :letter (nth matches 3)
     }))

(defn- parse-line
  [line]
  (let [matches (re-find #"([0-9]+-[0-9]+ [a-z]): ([a-z]+)$" line)
        condition (parse-condition (nth matches 1))]
    {:condition condition
     :password (nth matches 2)}))

(defn- validate-password-frequency
  [condition-password]
  (let [frequencies (frequencies (:password condition-password))
        min (get-in condition-password [:condition :min])
        max (get-in condition-password [:condition :max])
        letter (get-in condition-password [:condition :letter])
        letter-frequency (get frequencies letter)]
    (and (>= letter-frequency min) (<= letter-frequency max))))

(def frequency-validity (map #(validate-password-frequency (parse-line %)) input))

(count (filter #(identity %) frequency-validity))
