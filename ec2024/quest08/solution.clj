(ns quest08.solution
  (:require [clojure.math :as math]))


;; I
(defn part1 [n]
  (let [square (math/round (math/sqrt n))
        diff (- (math/pow square 2) n)
        width (dec (* 2 square))]
    (int (* diff width))))

(do
  (assert (= 21 (part1 13)))
  (prn :part1 (part1 4098734)))


;; II
(defn part2 [priests acolytes available]
  (loop [layer 2, total-blocks 1, prev-thick 1]
    (if (> total-blocks available)
      (let [diff (- total-blocks available)
            width (dec (* 2 (dec layer)))]
        (* diff width))
      (let [new-thick (mod (* prev-thick priests) acolytes)
            width (dec (* 2 layer))
            blocks (* new-thick width)]
        (recur (inc layer) (+ total-blocks blocks) new-thick)))))

(do
  (assert (= 27 (part2 3 5 50)))
  (prn :part2 (part2 959 1111 20240000)))


;; III
(defn rm-blocks [heights priests acolytes]
  (let [wid (dec (* 2 (count heights)))
        rm (fn [h] (mod (* h (* priests wid)) acolytes))
        half (map rm (butlast heights))]
    (or (apply + (first half) (map #(* 2 %) (rest half))) 0)))

(defn part3 [priests acolytes available]
  (loop [layer 2, total-blocks (bigint 1), prev-thick 1, heights [1]]
    (let [to-remove (rm-blocks heights priests acolytes)]
      (if (> (- total-blocks to-remove) available)
        (- (- total-blocks to-remove) available)
        (let [new-thick (+ (mod (* prev-thick priests) acolytes) acolytes)
              new-heights (conj (mapv #(+ % new-thick) heights) new-thick)
              blocks (apply + (first new-heights) (map #(* 2 %) (rest new-heights)))]
          (recur (inc layer), blocks, new-thick, new-heights))))))

(do
  (assert (= 2 (part3 2 5 160)))
  (prn :part3 (part3 944347 10 202400000)))
