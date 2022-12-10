#!/usr/bin/env bb

(def input (slurp "./input"))
(def instruction-regex #"(\w+)[\s]?([-]?\d*)")
(def signal-strength-indices (iterate #(+ % 40) 20))
(def crt-positions (range 0 40))

(defn- parse-instruction [instruction]
  (let [parsed (re-find instruction-regex instruction)
        command (nth parsed 1)
        param (nth parsed 2)]
    (case command
      "addx" {:duration 2 :function #(+ % (Integer/parseInt param))}
      "noop" {:duration 1 :function #(identity %)})))

(defn- parse-instructions [input]
  (->> input
       (str/split-lines)
       (map #(parse-instruction %))))

(defn- perform-instruction [acc curr]
  (as-> acc $
    (apply conj $ (repeat (- (:duration curr) 1) (first $)))
    (conj $ ((:function curr) (first $)))))

(defn- perform-instructions [instructions start-value]
  (reduce (fn [acc curr] (perform-instruction acc curr)) (list start-value) instructions))

(defn- get-sprite-range [pos]
  (range (- pos 1) (+ pos 2)))

(defn- get-drawn-pixels-for-row [sprites]
  (map-indexed (fn [index curr] (some #(= curr %) (nth sprites index))) crt-positions))

(def part-one
  (let [instructions (parse-instructions input)
        result (reverse (perform-instructions instructions 1))]
    (->> (take-while #(< % (count result)) signal-strength-indices)
         (map #(* (nth result (- % 1)) %))
         (reduce +))))

(def part-two
  (let [instructions (parse-instructions input)
        result (reverse (perform-instructions instructions 1))
        pixels (map #(get-sprite-range %) result)]
    (->> (partition 40 pixels)
         (map #(get-drawn-pixels-for-row %))
         (map (fn [row] (map #(if % "#" ".") row)))
         (map #(apply str %))
         (str/join "\n"))))

;;(spit "./output" part-two)
