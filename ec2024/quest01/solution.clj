(ns quest01.solution
  (:require [tools.input :refer [file->str]]))

(def CREATURE->POTIONS {\A 0, \B 1, \C 3, \D 5})

(defn solution [part & {:keys [notes]}]
  (let [input (or notes (file->str (format "input%d" part)))]
    (->> input
         (partition part)
         (map (fn [creatures] (keep #(when (not= % \x) (CREATURE->POTIONS %)) creatures)))
         (reduce (fn [acc potions] (apply + acc (get {2 2, 3 6} (count potions) 0) potions))
                 0))))

(comment
  (assert (= (solution 1 {:notes "ABBAC"}) 5))
  (solution 1) ; => 1408

  (assert (= (solution 2 {:notes "AxBCDDCAxD"}) 28))
  (solution 2) ; => 5560

  (assert (= (solution 3 {:notes "xBxAAABCDxCC"}) 30))
  (solution 3) ; => 27922
  )
