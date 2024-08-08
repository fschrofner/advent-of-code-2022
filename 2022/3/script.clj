#!/usr/bin/env bb

(def input (slurp "./input"))

(defn- get-value [symbol]
  (let [int-value (int symbol)]
    (if (>= int-value 97)
      (- int-value 96)
      (- int-value 38))))

(def rucksack-priorities
  (->> (str/split-lines input)
    (map #(seq %))
    (map #(split-at (/ (count %) 2) %))
    (map #(set/intersection (set (first %)) (set (second %))))
    (map #(get-value (first %)))))

(def group-priorities
  (->> (str/split-lines input)
       (map #(set (seq %)))
       (partition 3)
       (map #(set/intersection (first %) (second %) (nth % 2)))
       (map #(get-value (first %)))))

(def part-1 (reduce + rucksack-priorities))
(def part-2 (reduce + group-priorities))
