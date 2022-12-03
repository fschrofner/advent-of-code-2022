#!/usr/bin/env bb

(def input (slurp "./input"))

(def scores {"X" 1 "Y" 2 "Z" 3})
(def equals {"X" "A" "Y" "B" "Z" "C"})
(def wins {"X" "C" "Y" "A" "Z" "B"})

(defn- get-by-value [map target-value]
  (first (first (filter (fn [[key value]] (= value target-value)) map))))

(defn- get-duel-score [opponent player]
  (let [key player]
    (cond
      (= (get wins key) opponent) 6
      (= (get equals key) opponent) 3
      :else 0)))

(defn- get-symbol-score [player]
  (let [key player]
    (get scores key)))

(defn- get-player-value [opponent strategy]
  (case strategy
    "X" (let [winning-val (get-by-value wins opponent)
              draw-val (get-by-value equals opponent)]
          (first (filter #(and (not= % winning-val) (not= draw-val %)) (keys scores))))
    "Y" (get-by-value equals opponent)
    "Z" (get-by-value wins opponent)))

(defn- calculate-score
  ([line]
   (calculate-score line false))
  ([line calculate-player-value]
   (let [values (map str (apply vector (seq line)))
         opponent (nth values 0)
         player (nth values 2)
         player-value (if calculate-player-value (get-player-value opponent player) player)]
     (+ (get-duel-score opponent player-value) (get-symbol-score player-value)))))

(defn- calculate-total-score
  [calculate-player-value]
  (->> input
       (str/split-lines)
       (map #(calculate-score % calculate-player-value))
       (reduce +)))

(def part-1 (calculate-total-score false))
(def part-2 (calculate-total-score true))

