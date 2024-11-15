(ns tools.grid)

(defn make [input]
  (first
   (reduce
    (fn [[g [y x]] ch]
      (if (= ch \newline)
        [g [(inc y) 0]],
        [(assoc g [y x] (case ch \. 0 \# 1)) [y (inc x)]]))
    [(sorted-map) [0 0]] input)))

(defn show [g]
  (let [kx (keys g)
        yx (map first kx), xx (map second kx)
        Y (apply max yx), X (apply max xx)]
    (print
     (apply str
            (for [y (range (inc Y)) x (range (inc X)) :let [ch (g [y x])]]
              (str (if (= 0 ch) "." ch) (if (= x X) "\n" "")))))))

(defn adjacent-xy [[y x]]
   [[y (inc x)] [y (dec x)] [(inc y) x] [(dec y) x]])

(defn adjacent-all [point]
  (for [x [-1 0 1] y [-1 0 1] :when (not= [x y] [0 0])] (mapv + [x y] point)))
