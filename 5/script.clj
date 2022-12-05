#!/usr/bin/env bb

(def input (slurp "./input"))
(def move-regex #"move (\d+) from (\d+) to (\d+)")

(defn- create-field-lines [input]
  (->> input
       (str/split-lines)
       (map #(seq %))
       (reverse)))

(defn- create-field-indices [line]
  (->> (map-indexed (fn [index item] (if (Character/isDigit item) index nil)) line)
       (filter #(identity %))))

(defn- get-crates-at-index [lines index]
  (->> (map #(nth % index) lines)
       (filter #(Character/isLetter %))))

(defn- create-field [input-string]
  (let [lines (create-field-lines input-string)
        indices (create-field-indices (first lines))]
    (map #(get-crates-at-index (drop 1 lines) %) indices)))

(defn- parse-moves [move-lines]
  (->> (str/split-lines move-lines)
       (map #(as-> (re-find move-regex %) $
               {:amount (Integer/parseInt (nth $ 1))
                :from (Integer/parseInt (nth $ 2))
                :to (Integer/parseInt (nth $ 3))
                }))))

(defn- perform-move [field from to amount]
  (let [from-index (- from 1)
        to-index (- to 1)
        items (take-last amount (nth field from-index))]
    (map-indexed (fn [index stack]
                   (cond
                     (= index from-index) (drop-last amount stack)
                     (= index to-index) (concat stack items)
                     :else stack)) field)))

;;iterate creates a lazy sequence of applying that function recursively, while nth picks a specific result from that lazy sequence
(defn- perform-moves [parsed-field parsed-moves move-multiple]
  (reduce (fn [field move]
            (nth (iterate (fn [field]
                            (perform-move field (:from move) (:to move) (if move-multiple (:amount move) 1))) field) (if move-multiple 1 (:amount move)))) parsed-field parsed-moves))

(defn- calculate-final-field [move-multiple]
  (let [[field moves] (str/split input #"\n\n")
        parsed-field (create-field field)
        parsed-moves (parse-moves moves)]
    (->> (perform-moves parsed-field parsed-moves move-multiple)
         (map #(last %)))))

(def part-one (calculate-final-field false))
(def part-two (calculate-final-field true))
