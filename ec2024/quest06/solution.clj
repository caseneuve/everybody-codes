(ns quest06.solution
  (:require [tools.io :refer [file->str solve]]
            [clojure.string :refer [split-lines]]))

(defn mktree [it]
  (->> it
       split-lines
       (map #(re-seq #"\w+|@" %))
       (reduce
        (fn [tree line]
          (let [[node children] ((juxt first rest) line)]
            (assoc tree node children)))
        {})))

(defn solution [part & {:keys [test-input]}]
  (let [tree (mktree (or test-input (file->str 6 (str "input" part))))
        root "RR"
        describe (if (= part 1) identity first)]
    (loop [Q (into clojure.lang.PersistentQueue/EMPTY [[root]]), fruit-branches []]
      (if (empty? Q)
        (->> fruit-branches (group-by count) vals
             (keep #(when (= 1 (count %)) (->> % first (map describe) (apply str))))
             first)
        (let [cur (peek Q)
              found (reduce
                     (fn [acc node]
                       (update acc (if (= node "@") :fruit :branch) conj (conj cur node)))
                     {:fruit [] :branch []}
                     (remove #(= % "BUG") (tree (last cur))))]
          (recur (into (pop Q) (found :branch)), (into fruit-branches (found :fruit))))))))


(comment
  (let [test "RR:A,B,C
A:D,E
B:F,@
C:G,H
D:@
E:@
F:@
G:@
H:@"]
    (assert (= (solution 1 {:test-input test}) "RRB@"))

    {:1 (solution 1) ;; => "RRWWVMMQDMMC@"
     :2 (solution 2) ;; => "RHWHKQMMPV@"
     :3 (solution 3) ;; => "RPMNGXRKWBMF@"
     })
  )

(defn -main [part] (solve part [#(solution 1), #(solution 2), #(solution 3)]))
