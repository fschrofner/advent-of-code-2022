#!/usr/bin/env bb

(def input (slurp "./input"))

;;splits the list into multiple sublists based on the element fulfilling the predicate. that element will not be included in the result.
(defn- split-by-element [pred items]
  (reduce (fn [acc curr]
            (if (pred curr)
              (conj acc [])
              (assoc acc (- (count acc) 1) (conj (last acc) curr)))) [[]] items))

(def sorted (->> input
    (str/split-lines)
    (split-by-element #(empty? %))
    (map #(reduce (fn [acc curr] (+ acc (Integer/parseInt curr))) 0 %))
    (sort)
    (reverse)))

(def first-elf (first sorted))
(def top-three-elves (->> (take 3 sorted)
                         (reduce +)))
