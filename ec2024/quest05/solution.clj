(ns quest05.solution
  (:require [tools.input :refer [file->str]]
            [clojure.edn :refer [read-string]]))

(defn parse [it]
  (->> it
       (re-seq #"\d+")
       (map read-string)
       (partition 4)
       (apply mapv vector)))

(defn dance [dancers round]
  (let [cur-idx (mod round 4), cur-col (dancers cur-idx)
        clapper (first cur-col)
        nxt-idx (mod (inc round) 4), nxt-col (dancers nxt-idx)
        direction-changes (quot clapper (count nxt-col))
        claps-remaining (rem clapper (count nxt-col))
        transform-fn (if (even? direction-changes) identity reverse)
        temp-col (transform-fn nxt-col)
        absorb-col (transform-fn
                    (concat
                     (take (abs (dec claps-remaining)) temp-col)
                     [clapper]
                     (drop (abs (dec claps-remaining)) temp-col)))]
    (-> dancers
        (assoc cur-idx (vec (rest cur-col)))
        (assoc nxt-idx (vec absorb-col)))))

(defn part1 [dancers]
  (loop [D dancers, round 0, shout nil]
    (if (= round 10) shout
        (let [new-order (dance D round)
              shout (apply str (map first new-order))]
          (recur new-order, (inc round), shout)))))

(defn part2 [dancers]
  (loop [D dancers, round 0, repetitions {}]
    (let [new-order (dance D round)
          shout (apply str (map first new-order))
          repeated (inc (get repetitions shout 0))]
      (if (= 2024 repeated)
        (* (read-string shout) (inc round))
        (recur new-order (inc round) (assoc repetitions shout repeated))))))

(defn part3 [dancers]
  (loop [D dancers, round 0, highest 0]
    (if (= round 100) highest
        (let [new-order (dance D round)
              shout (read-string (apply str (map first new-order)))]
          (recur new-order (inc round) (max shout highest))))))


(comment
  (let [test1   (parse "2 3 4 5\n3 4 5 2\n4 5 2 3\n5 2 3 4")
        test2-3 (parse "2 3 4 5\n6 7 8 9")
        it     #(parse (file->str (str "input" %)))]

    (assert (= "2323"   (part1 test1)))
    (assert (= 50877075 (part2 test2-3)))
    (assert (= 6584     (part3 test2-3)))

    {:1 (part1 (it 1))  ;; => "2233"
     :2 (part2 (it 2))  ;; => 17386198904492
     :3 (part3 (it 3))  ;; => 9289100310028839
     })
  )
