#!/usr/bin/env bb

(def input (slurp "./input"))

(defn- get-ranges [line]
  (->> (str/split line #",")
       (map #(str/split % #"-"))
       (map (fn [v] (map #(Integer/parseInt %) v)))
       (map #(range (first %) (+ (second %) 1)))))

(defn- contains [range-1 range-2]
  (let [set-1 (set range-1)
        set-2 (set range-2)]
    (or (set/subset? set-1 set-2) (set/superset? set-1 set-2))))

(defn- overlaps [range-1 range-2]
  (let [set-1 (set range-1)
        set-2 (set range-2)]
    (seq (set/intersection set-1 set-2))))

(def part-one
  (->> (str/split-lines input)
       (map #(get-ranges %))
       (map #(if (apply contains %) 1 0))
       (reduce +)))

(def part-two
  (->> (str/split-lines input)
       (map #(get-ranges %))
       (map #(if (apply overlaps %) 1 0))
       (reduce +)))
