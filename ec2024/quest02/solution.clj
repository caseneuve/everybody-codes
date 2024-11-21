(ns quest02.solution
  (:require [clojure.string :refer [trim split-lines split replace]]))

(defn get-input [part]
  (trim (slurp (format "/home/piotr/git/everybodycodes/ec2024/quest02/input%d.txt" part))))


(comment
  (let [ ;;         ii "WORDS:THE,OWE,MES,ROD,HER

        ;; AWAKEN THE POWER ADORNED WITH THE FLAMES BRIGHT IRE"

        ii "WORDS:THE,OWE,MES,ROD,HER

AWAKEN THE POWE ADORNED WITH THE FLAMES BRIGHT IRE
THE FLAME SHIELDED THE HEART OF THE KINGS
POWE PO WER P OWE R
THERE IS THE END"
        ii (get-input 2)
        [words s] (split ii #"\n\n")
        ;; 2
        ww (rest (re-seq #"\w+" words))
        ;; ll "AWAKEN THE POWE ADORNED WITH THE FLAMES BRIGHT IRE"
        ]

    ;; p I
    ;; (->> words (re-seq #"\w+") rest
    ;;      (reduce (fn [acc w] (+ acc (count (re-seq (re-pattern w) s)))) 0)
    ;;      )

    ;; p II
    (loop [lx (split-lines s), c 0]
      (if (empty? lx)
        c
        (let [ll (first lx)
              mx (for [w ww, ln [ll (apply str (reverse ll))]
                       :let [r (replace ln w (apply str (repeat (count w) \1)))]]
                   (if (= ln ll) r (apply str (reverse r))))
              nx (map-indexed (fn [idx _] (if (some #(= \1 (get % idx)) mx) 1 0)) ll)]
          (recur (rest lx) (apply + c nx))))))

  5470

  ;; p3
  (let [ii "WORDS:THE,OWE,MES,ROD,RODEO

HELWORLT
ENIGWDXL
TRODEOAL"
        ;; ii (get-input 3)
        [words s] (split ii #"\n\n")
        ww (rest (re-seq #"\w+" words))
        ]

    ;; p II
    (loop [lx (split-lines s), c 0]
      (if (empty? lx)
        c
        (let [ll (first lx)
              mx (for [w ww, ln [ll (apply str (reverse ll))]
                       :let [r (replace ln w (apply str (repeat (count w) \1)))]]
                   (if (= ln ll) r (apply str (reverse r))))
              nx (map-indexed (fn [idx _] (if (some #(= \1 (get % idx)) mx) 1 0)) ll)]
          (recur (rest lx) (apply + c nx))))))

  5470

  ;; (let [s "HELWORLT"
  ;;       p "THE"
  ;;       e (apply str (take (+ (count s) (dec (count p))) (cycle s)))
  ;;       o (replace e p (apply str (repeat (count p) \1)))
  ;;       suf (drop (count s) o)
  ;;       mid (drop (dec (count p)) (take (count s) o))]
  ;;   (apply str (concat suf mid)))

  (defn left-right [s ww]
    (let [slen (count s)
          matches (for [ln [s (apply str (reverse s))]
                        ;; p ["THE" , "OWE" , "MES" , "ROD" , "RODEO"]
                        p ww
                        :let [plen (count p)
                              e (apply str (take (+ slen (dec plen)) (cycle ln)))
                              o (replace e p (apply str (repeat plen \1)))]
                        :when (some #(= \1 %) o)]
                    (let [rd (reduce
                              (fn [[idx acc] ch]
                                (let [new (if (= ch \1) (assoc acc (mod idx slen) 1) acc)]
                                  [(inc idx), new]))
                              [0 (vec (repeat slen 0))]
                              o)]
                      (second rd)))]
      (map-indexed (fn [idx _] (if (some #(= 1 (get % idx)) matches) 1 0)) s)))


  (defn top-bottom [input ww]
    (apply mapv vector
           (let [lxx (apply mapv str input)
                 ;; ww ["THE" , "OWE" , "MES" , "ROD" , "RODEO"]
                 ]
             (loop [lx lxx, c []]
               (if (empty? lx)
                 c
                 (let [ll (first lx)
                       mx (for [w ww, ln [ll (apply str (reverse ll))]
                                :let [r (replace ln w (apply str (repeat (count w) \1)))]]
                            (if (= ln ll) r (apply str (reverse r))))
                       nx (map-indexed (fn [idx _] (if (some #(= \1 (get % idx)) mx) 1 0)) ll)]
                   (recur (rest lx) (conj c nx)))))
             )))



  (let [ii (get-input 3)
        [words s] (split ii #"\n\n")
        ww (rest (re-seq #"\w+" words))
        lines (split-lines s)
        lr (for [l lines :let [mx (left-right l ww)] :when (seq mx)] mx)
        tb (top-bottom lines ww)
        aligned (mapv vector lr tb)
        merged (map
                (fn [mx]
                  (map-indexed
                   (fn [idx _] (if (some #(= 1 (get (vec %) idx)) mx) 1 0))
                   (first mx)))
                aligned)
        ]
    merged
    ;; (apply + (flatten merged))
    )
  15760


  )
