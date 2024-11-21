(ns quest03.solution
  (:require [tools.io :refer [file->str solve]]
            [tools.grid :as grid]))

(defn dig [old adjacent-fn]
  (loop [[key & keys] (keys old), new (sorted-map), changed false]
    (if (nil? key) [changed new]
        (let [cur-lvl (old key)]
          (if (zero? cur-lvl)
            (recur keys (assoc new key 0) changed)
            (let [dig? (every? #(= cur-lvl %) (for [point (adjacent-fn key)] (old point 0)))]
              (recur keys (assoc new key (if dig? (inc cur-lvl) cur-lvl)) (or changed dig?))))))))

(defn solution [part & {:keys [notes]}]
  (let [adj-fn (if (= part 3) grid/adjacent-all grid/adjacent-xy)
        input (or notes (file->str 3 (format "input%d" part)))]
    (loop [g (grid/make input {:parse-fn #(case % \. 0 \# 1)})]
      (let [[changed? new] (dig g adj-fn)]
        (if changed? (recur new) (->> new vals (apply +)))))))

(comment
  (let [part1 "..........
..###.##..
...####...
..######..
..######..
...####...
.........."]
    (assert (= 35 (solution 1 {:notes part1}))))

  (solution 1) ;; => 129
  (solution 2) ;; => 2789

  (let [part3 "..........
..###.##..
...####...
..######..
..######..
...####...
.........."]
    (assert (= 29 (solution 3 {:notes part3}))))

  (solution 3) ;; => 10427
  )

(defn -main [part] (solve part [#(solution 1), #(solution 2), #(solution 3) ]))
