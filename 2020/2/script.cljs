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

(defn- validate-password-position
  [condition-password]
  (let [password (:password condition-password)
        min (get-in condition-password [:condition :min])
        max (get-in condition-password [:condition :max])
        letter (get-in condition-password [:condition :letter])
        letter-at-min-match (= letter (nth password (- min 1)))
        letter-at-max-match (= letter (nth password (- max 1)))]
    (and (or letter-at-min-match letter-at-max-match) (not (and letter-at-min-match letter-at-max-match)))))

(def frequency-validity (map #(validate-password-frequency (parse-line %)) input))

(def position-validity (map #(validate-password-position (parse-line %)) input))

(count (filter #(identity %) frequency-validity))
(count (filter #(identity %) position-validity))
