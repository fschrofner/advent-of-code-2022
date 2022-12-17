#!/usr/bin/env bb

(def input (slurp "./input"))

(def pairs
  (as-> input $
    (str/split $ #"\n\n")
    (map #(str/split-lines %) $)
    (map (fn [pair] (map #(json/parse-string %) pair)) $)))

(defn- compare-lists [left right]
  (let [left-size (count left)
        right-size (count right)]
    (as-> (interleave left right) $
      (partition 2 $)
      (map (fn [pair] (compare-value (first pair) (second pair))) $)
      (some #(if (not (= % :unknown)) % nil) $)
      (if (nil? $)
        (cond
          (> left-size right-size) :unordered
          (< left-size right-size) :ordered
          :else :unknown) $))))

(defn- compare-value [left right]
  (cond
    (and (sequential? left) (sequential? right)) (compare-lists left right)
    (sequential? left) (compare-lists left (list right))
    (sequential? right) (compare-lists (list left) right)
    :else (cond
            (< left right) :ordered
            (> left right) :unordered
            :else :unknown)))

(def part-one
  (->> pairs
       (keep-indexed (fn [index item] (if (= (compare-value (first item) (second item)) :ordered) (+ index 1))))
       (reduce +)))

(def divider-packet-one [[2]])
(def divider-packet-two [[6]])
(def packets
  (as-> input $
    (str/replace $ #"\n\n" "\n")
    (str/split-lines $) 
    (map #(json/parse-string %) $)
    (conj $ divider-packet-one)
    (conj $ divider-packet-two)))

(def part-two
  (->> packets
       (sort (fn [left right]
               (let [result (compare-value left right)]
                 (case result
                   :ordered -1
                   :unknown 0
                   :unordered 1))))
       (keep-indexed (fn [index item]
                       (if (or (= item divider-packet-one)
                               (= item divider-packet-two)) (+ index 1))))
       (apply *)))
