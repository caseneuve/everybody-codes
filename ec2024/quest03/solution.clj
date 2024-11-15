(ns quest03.solution
  (:require [tools.input :refer [file->str]]
            [tools.grid :as grid]))

(defn dig [old adjacent-fn]
  (loop [[key & keys] (keys old), new (sorted-map), changed false]
    (if (nil? key) [changed new]
        (let [cur-lvl (old key)]
          (if (zero? cur-lvl)
            (recur keys (assoc new key 0) changed)
            (let [dig? (every? #(= cur-lvl %) (for [point (adjacent-fn key)] (old point 0)))]
              (recur keys (assoc new key (if dig? (inc cur-lvl) cur-lvl)) (or changed dig?))))))))

(defn solution [part & {:keys [test-input]}]
  (let [adj-fn (if (= part 3) grid/adjacent-all grid/adjacent-xy)
        input (or test-input (file->str 3 part))]
    (loop [g (grid/make input)]
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
    (assert (= 35 (solution 1 {:test-input part1}))))

  (solution 1) ;; => 129
  (solution 2) ;; => 2789

  (let [part3 "..........
..###.##..
...####...
..######..
..######..
...####...
.........."]
    (assert (= 29 (solution 3 {:test-input part3}))))

  (solution 3) ;; => 10427
  )
