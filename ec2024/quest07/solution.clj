(ns quest07.solution
  (:require [tools.io :refer [file->str solve]]
            [clojure.string :refer [split-lines]]
            [tools.grid :as grid]
            [clojure.math.combinatorics :as combo]))

(def ACTIONS {"+" inc "-" dec "=" identity})

(defn strategies [it]
  (->> it
       split-lines
       (reduce (fn [acc line]
                 (let [[chariot & actions] (re-seq #"\w|\+|-|=" line)]
                   (assoc acc chariot (vec actions))))
               {})))

(defn score [track strategy round initial-power]
  (->
   (reductions
    (fn [power [idx t-act]]
      (let [act (ACTIONS
                (if (contains? #{"=" "S"} t-act)
                  (get (vec strategy) (mod (+ (* round (count track)) idx) (count strategy)))
                  t-act))]
        (act power)))
    initial-power track)
   rest
   (as-> powers [(apply + powers) (last powers)])))

(defn part1 [& {:keys [notes]}]
  (let [S (->> (or notes (file->str 7 "input1")) strategies)
        track (map-indexed vector (repeat 10 "="))]
    (->> S
         (reduce
          (fn [acc [chariot strategy]]
            (assoc acc chariot (first (score track strategy 0 10)))) {})
         (sort-by second >)
         (map first)
         (apply str))))

(defn mktrack [s]
  (let [G (grid/make s {:parse-fn str})]
    (loop [pos [0 1], seen #{[0 0]}, track [(G pos)]]
      (if (nil? pos) (conj track "S")
        (let [cand (->> pos
                        grid/adjacent-xy
                        (filter #(contains? (set (keys ACTIONS)) (G %)))
                        (remove #(contains? seen %)))
              move-to (do (assert (< (count cand) 2) (format "should be no more than one: %s" (list cand)))
                          (first cand))
              act (G move-to)]
          (recur move-to, (conj seen pos), (cond-> track act (conj act))))))))

(defn part2 [& {:keys [notes track]}]
  (let [S (strategies (or notes (file->str 7 "input2")))
        T (map-indexed vector (mktrack (or track (file->str 7 "track2"))))]
    (loop [round 0, results (zipmap (keys S) (repeat [0 10]))]
      (if (= round 10)
        (->> results
             (map (fn [[k vx]] [k (apply + vx)]))
             (sort-by second >)
             (map first)
             (apply str))
        (recur (inc round)
               (merge-with
                concat
                results
                (into
                 {}
                 (for [chariot (keys S) :let [strategy (S chariot), init-power (last (results chariot))]]
                   [chariot (score T strategy round init-power)]))))))))

(defn part3 []
  (let [rival-plan (-> (strategies (file->str 7 "input3")) (get "A"))
        track (mapv vector (range) (mktrack (file->str 7 "track3")))
        race-result-for (fn [strategy]
                          (loop [round 0, result 0, power 10]
                            (if (= round (count strategy)) result ;; since (= 0 (mod 2024 (count strategy)))
                                (let [[essence last-power] (score track strategy round power)]
                                  (recur (inc round) (+ result essence) last-power)))))
        rival-score (race-result-for rival-plan)
        all-plans (combo/permutations rival-plan)
        ]
    (->> all-plans
         (pmap race-result-for)
         (filter #(> % rival-score))
         count)))

(comment

  (let [test1 "A:+,-,=,=
B:+,=,-,+
C:=,-,+,+
D:=,=,=,+"
        track2 "S+===
-   +
=+=-+"]
    (assert (= "BDCA" (part1 {:notes test1})))
    (assert (= "DCBA" (part2 {:notes test1 :track track2}))))

  (part1) ;;=> "KDJABECGH"
  (part2) ;;=> "JBDAHGIKE"
  (part3) ;;=> 5797
  )

(defn -main [part] (solve part [part1 part2 part3]))
