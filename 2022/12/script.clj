#!/usr/bin/env bb

(def input (slurp "/home/schrofi/Projects/advent-of-code-2022/12/input"))
(def start-marker (int \S))
(def end-marker (int \E))

(defn- parse-field
  [input]
  (->> input
       (str/split-lines)
       (map #(into [] (seq %)))
       (mapv (fn [row] (into [] (map #(int %) row))))))

(def parsed-field (parse-field input))
(def field-width (count (first parsed-field)))
(def field-height (count parsed-field))

(defn- find-coordinates-for-marker [field marker]
  (let [indices (keep-indexed #(if (= %2 marker) %1) (flatten field))]
    (map #(vector (mod % field-width) (int (/ % field-width))) indices)))

(defn- find-coordinate-for-marker [field marker]
  (first (find-coordinates-for-marker field marker)))

(def start-position (find-coordinate-for-marker parsed-field start-marker))
(def end-position (find-coordinate-for-marker parsed-field end-marker))

(def field
  (-> parsed-field
      (assoc-in [(second start-position)(first start-position)] (int \a))
      (assoc-in [(second end-position) (first end-position)] (int \z))
      ))

(defn- get-value-for-position [position]
  (let [x (first position)
        y (second position)]
    (get-in field [y x])))

(defn- is-valid-position [position]
  (let [x (first position)
        y (second position)]
    (and (>= x 0) (< x field-width) (>= y 0) (< y field-height))))

(defn- get-valid-moves [position]
  (let [x (first position)
        y (second position)
        moves (vector (vector x (- y 1))
                      (vector x (+ y 1))
                      (vector (- x 1) y)
                      (vector (+ x 1) y))]
    (filter #(is-valid-position %) moves)))

(defn- get-possible-moves [position possible-field-check]
  (let [value (get-value-for-position position)
        valid-moves (get-valid-moves position)]
    (filter (fn [position] (possible-field-check value (get-value-for-position position))) valid-moves)))

;;breadth first search is not really efficient nor is this implementation any good, but at least it returns the correct results
;;so i'm gonna leave it at that
(defn- get-distance-to-field [target-check current-fields depth visited-fields possible-field-check]
  (let [seen-fields (into visited-fields current-fields)]
    (if (some #(target-check %) current-fields)
      depth
      (let [possible-targets (distinct (apply concat (map #(get-possible-moves % possible-field-check) current-fields)))
            filtered-targets (filter (fn [target] (false? (some #(= % target) seen-fields))) possible-targets)]
        (recur target-check filtered-targets (+ depth 1) seen-fields possible-field-check)))))

(def part-one
  (get-distance-to-field #(= % end-position) (list start-position) 0 '() (fn [value other-value] (>= (- value other-value) -1))))

;;basically doing the search in the other direction
(def part-two
  (let [targets (find-coordinates-for-marker field (int \a))]
    (get-distance-to-field (fn [field] (some #(= % field) targets)) (list end-position) 0 '() (fn [value other-value] (>= (- other-value value) -1)))))
