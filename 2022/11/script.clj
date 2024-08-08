#!/usr/bin/env bb

(def input (slurp "/home/schrofi/Projects/advent-of-code-2022/11/input"))
(def items-regex #"Starting items: (\d+[, \d+]*)")
(def operation-regex #"Operation: new = (\w+) (\p{Punct}) (\w+)")
(def number-regex #"\d+")
(def divisible-regex #"Test: divisible by (\d+)")
(def condition-regex #"If (\p{Lower}+): throw to monkey (\d+)")

(defn- get-all-regex-matches [regex string]
  (->> (re-seq regex string)
       (map #(drop 1 %))
       (flatten)))

(defn- parse-starting-items [string]
  (as-> (re-find items-regex string) $
    (last $)
    (str/split $ #", ")
    (map #(bigint (Integer/parseInt %)) $)
    (reverse $)))

(defn- parse-command-element [element-string]
  (cond
    (not (nil? (re-matches number-regex element-string))) (fn [x] (Integer/parseInt element-string))
    (= element-string "old") (fn [x] x)
    :else nil))

(defn- parse-command [command-string param1-string param2-string]
  (let [command (case command-string
                  "*" *
                  "+" +
                  "-" -)]
    (fn [x] (command ((parse-command-element param1-string) x) ((parse-command-element param2-string) x)))))

(defn- parse-operation [string]
  (let [command-match (re-find operation-regex string)]
    (parse-command (nth command-match 2) (nth command-match 1) (nth command-match 3))))

(defn- parse-divisor [string]
  (Integer/parseInt (last (re-find divisible-regex string))))

(defn- parse-condition [divisor string]
  (let [condition-matches (get-all-regex-matches condition-regex string)
        condition-map {
                       (keyword (nth condition-matches 0))
                       (Integer/parseInt (nth condition-matches 1))
                       (keyword (nth condition-matches 2))
                       (Integer/parseInt (nth condition-matches 3))
                       }]
    (fn [x]
      (if (= (mod x divisor) 0)
        (:true condition-map)
        (:false condition-map)))))


(defn- parse-monkey [string]
  (let [divisor (parse-divisor string)]
    {
     :items (parse-starting-items string) ;;list of worry levels of the current items
     :operation (parse-operation string) ;;function taking a worry level and returning the updated worry level
     :divisor divisor ;;divisor used for checking the next monkey to pass the item to
     :condition (parse-condition divisor string) ;;function taking a worry level and returning the next target monkey
     :inspections 0 ;;counter of inspected items
     }))

(defn- parse-monkeys [string]
  (as-> string $
    (str/split $ #"\n\n")
    (map #(parse-monkey %) $)))

(defn- handle-operation [value monkey monkeys worry-handling]
  (let [new-value (bigint (worry-handling ((:operation monkey) value)))
        target ((:condition monkey) new-value)]
    (map-indexed (fn [index item] (if (= index target) (assoc-in item [:items] (conj (get-in item [:items]) new-value)) item)) monkeys)))

;;removes items from monkey and performs all operations in order, also increases amount of inspections
(defn- handle-monkey-operations [index monkeys worry-handling]
  (let [monkey (nth monkeys index)
        items (reverse (:items monkey))
        inspections (:inspections monkey)
        updated-monkeys (map-indexed
                         #(if (= index %1)
                            (-> %2
                                (assoc-in [:items] '())
                                (assoc-in [:inspections] (+ inspections (count items)))) %2) monkeys)]
    (reduce (fn [acc curr] (handle-operation curr monkey acc worry-handling)) updated-monkeys items)))

(defn- perform-round [monkeys worry-handling]
  (reduce #(handle-monkey-operations %2 %1 worry-handling) monkeys (range 0 (count monkeys))))

(def part-one
  (as-> input $
    (parse-monkeys $)
    (nth (iterate #(perform-round % #(/ % 3)) $) 20)
    (map #(:inspections %) $)
    (reverse (sort $))
    (* (first $) (second $))))


(def part-two
  (let [monkeys (parse-monkeys input)
        lcm (apply * (map #(:divisor %) monkeys))]
    (as-> monkeys $
      (nth (iterate #(perform-round % #(mod % lcm)) $) 10000)
      (map #(:inspections %) $)
      (reverse (sort $))
      (* (first $) (second $)))))

