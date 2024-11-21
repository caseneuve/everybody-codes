(ns tools.io
  (:require [clojure.string :refer [split-lines trim]]))

(defn file->str [part file]
  (trim (slurp (format "quest%02d/%s" part file))))

(defn file->lines [part file]
  (split-lines (file->str part file)))

(defn solve [part solution-fns]
  (for [p (range (count solution-fns)) :when (or (nil? part) (= (inc p) (Integer. (name part))))]
    [(inc p) ((solution-fns p))]))
