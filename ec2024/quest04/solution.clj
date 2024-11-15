(ns quest04.solution
  (:require
   [clojure.edn :refer [read-string]]
   [clojure.string :refer [split-lines]]
   [tools.input :refer [file->lines]]))

(defn smallest [nums]
  (apply min nums))

(defn median [nums]
  (get (vec (sort nums))
       (quot (count nums) 2)))

(defn solution [part & {:keys [test-input]}]
  (let [nums (map read-string (or test-input (file->lines 4 part)))
        sub-fn (if (= part 3) median smallest)]
    (apply + (map #(abs (- % (sub-fn nums))) nums))))

(comment
  (let [part1 (split-lines "3\n4\n7\n8")
        part2 (split-lines "2\n4\n5\n6\n8")]
    (assert (= 10 (solution 1 {:test-input part1})))
    (assert (=  8 (solution 3 {:test-input part2}))))

  (solution 1) ;; => 65
  (solution 2) ;; => 865607
  (solution 3) ;; => 127030805

  )
