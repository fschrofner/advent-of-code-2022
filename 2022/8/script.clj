#!/usr/bin/env bb

(def input (slurp "./input"))
(def field (->> input
                (str/split-lines)
                (map #(seq %))
                (clojure.walk/postwalk #(if (not (seq? %)) (Character/digit % 10) %))))

(defn- get-fields-for-condition [x-cond y-cond]
  (->> field
       (keep-indexed #(if (y-cond %1) %2))
       (map (fn [row] (keep-indexed #(if (x-cond %1) %2) row)))
       (flatten)))

(defn- get-max-for-condition [x-cond y-cond]
  (apply max (get-fields-for-condition x-cond y-cond)))

(defn- get-max-in-each-direction [x y]
  (let [left (get-fields-for-condition #(< % x) #(= % y))
        right (get-fields-for-condition #(> % x) #(= % y))
        top (get-fields-for-condition #(= % x) #(< % y))
        bottom (get-fields-for-condition #(= % x) #(> % y))]
    (list (apply max left) (apply max right) (apply max top) (apply max bottom))))

(defn- calculate-viewing-distance [value view-line]
  (if (= (count view-line) 0) 0
      (as-> (keep-indexed #(if (>= %2 value) %1) view-line) $
        (if (empty? $) (count view-line) (+ (first $) 1)))))

;;need to reverse as we're going top -> bottom & left -> right
(defn- get-viewing-distance [x y]
  (let [value (nth (nth field y) x)
    left (reverse (get-fields-for-condition #(< % x) #(= % y)))
    right (get-fields-for-condition #(> % x) #(= % y))
    top (reverse (get-fields-for-condition #(= % x) #(< % y)))
    bottom (get-fields-for-condition #(= % x) #(> % y))]
   (* (calculate-viewing-distance value left) (calculate-viewing-distance value right) (calculate-viewing-distance value top) (calculate-viewing-distance value bottom))))

(defn- is-visible [x y]
  (let [value (nth (nth field y) x)]
    (cond
      (> value (get-max-for-condition #(< % x) #(= % y))) true
      (> value (get-max-for-condition #(> % x) #(= % y))) true
      (> value (get-max-for-condition #(= % x) #(< % y))) true
      (> value (get-max-for-condition #(= % x) #(> % y))) true
      :else false)))

(def visibility-map
  (let [width (count field)
        height (count (nth field 0))]
    (for [y (range 1 (- height 1))
          x (range 1 (- width 1))]
      (is-visible x y))))

(def amount-of-border-trees
  (let [width (count field)
        height (count (nth field 0))]
    (- (+ (* width 2) (* 2 height)) 4)))

(def viewing-distance-map
  (let [width (count field)
        height (count (nth field 0))]
    (for [y (range 1 (- height 1))
          x (range 1 (- width 1))]
      (get-viewing-distance x y))))

(def part-one (+ (count (filter #(= % true) visibility-map)) amount-of-border-trees))
(def part-two (apply max viewing-distance-map))
