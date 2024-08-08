(local input (slurp "./test-data"))
(local input-lines (split-lines input))

(local size (length input-lines))

(fn find-pair-with-sum [list sum]
  (var result [])
  (for [main-index 1 size]
    (if (< main-index size)
        (for [other-index (+ main-index 1) size]
          (let [first (tonumber (. list main-index))
                second (tonumber (. list other-index))]
            (if (= sum (+ first second)) (set result (values [first second])))))))
  result)

(local pair (find-pair-with-sum input-lines 2020))

;;todo: find triplets with sum

(#(* $1 $2) (table.unpack pair))
(#(+ $1 $2) (table.unpack pair))
