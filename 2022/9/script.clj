#!/usr/bin/env bb

(def input (slurp "./input"))
(def movement-regex #"([LRUD]) (\d+)")

(defn- parse-direction [direction]
  (case direction
    "L" :left
    "R" :right
    "U" :up
    "D" :down))

(defn- parse-movement [string]
  (as-> (re-find movement-regex string) $
    (list (parse-direction (nth $ 1)) (Integer/parseInt (nth $ 2)))))

(defn- get-next-tail-position [head tail]
  (let [[head-x head-y] head
        [tail-x tail-y] tail
        x-diff (- head-x tail-x)
        y-diff (- head-y tail-y)]
    (cond
      (and (<= (abs x-diff) 1) (<= (abs y-diff) 1)) tail
      (and (>= (abs x-diff) 2) (= (abs y-diff) 0)) (list (+ tail-x (/ x-diff (abs x-diff)),) tail-y)
      (and (= (abs x-diff) 0) (>= (abs y-diff) 2)) (list tail-x (+ tail-y (/ y-diff (abs y-diff))))
      :else (list (+ tail-x (/ x-diff (abs x-diff))) (+ tail-y (/ y-diff (abs y-diff))))
      )))

(defn- perform-head-move [head move]
  (let [[head-x head-y] head
        [direction steps] move]
    (case direction
      :left (list (- head-x steps) head-y)
      :right (list (+ head-x steps) head-y)
      :up (list head-x (+ head-y steps))
      :down (list head-x (- head-y steps)))))

(defn- calculate-next-positions [rope move]
  (let [head (last rope)
        tails (drop-last 1 rope)
        next-head-position (perform-head-move head move)]
    (reduce (fn [acc curr] (conj acc (get-next-tail-position (first acc) curr))) (list next-head-position) (reverse tails))))

(defn- append-next-state [accumulate move]
  (let [current-state (first accumulate)
        [direction steps] move]
    (if (> steps 1)
      (nth (iterate #(append-next-state % (list direction 1)) accumulate) steps)
      (conj accumulate (calculate-next-positions current-state move)))))

(def movements
  (->> input
       (str/split-lines)
       (map #(parse-movement %))))

(defn- get-positions [start-list]
  (reduce (fn [acc curr] (append-next-state acc curr)) (list start-list) movements))

(defn- get-unique-tail-positions [start-list]
  (->> (get-positions start-list)
       (map #(first %))
       (distinct)
       (count)))

(def part-one
  (get-unique-tail-positions (repeat 2 '(0 0))))

(def part-two
  (get-unique-tail-positions (repeat 10 '(0 0))))
