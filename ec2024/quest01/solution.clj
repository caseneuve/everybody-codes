(ns quest01.solution
  (:require [clojure.string :refer [trim]]))

(defn get-input [part]
  (trim (slurp (format "input%d.txt" part))))

(def POTIONS {\A 0, \B 1, \C 3, \D 5})

(defn solution [part & {:keys [test-input]}]
  (let [input (or test-input (get-input part))]
    (->> input
         (partition part)
         (map (fn [gr] (keep #(when (not= % \x) (POTIONS %)) gr)))
         (reduce (fn [acc gr] (apply + acc (get {2 2, 3 6} (count gr) 0) gr))
                 0))))

(comment
  (assert (= (solution 1 {:test-input "ABBAC"}) 5))
  (solution 1) ; => 1408

  (assert (= (solution 2 {:test-input "AxBCDDCAxD"}) 28))
  (solution 2) ; => 5560

  (assert (= (solution 3 {:test-input "xBxAAABCDxCC"}) 30))
  (solution 3) ; => 27922
  )
