#!/usr/bin/env bb

(def input (slurp "./input"))

(defn- find-marker [string marker-size]
  (->> string
       (seq)
       (partition marker-size 1)
       (keep-indexed #(if (= (count (distinct %2)) marker-size) %1))
       (first)
       (+ marker-size)))

(def part-one (find-marker input 4))
(def part-two (find-marker input 14))
