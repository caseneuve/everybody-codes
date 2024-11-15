(ns tools.input
  (:require [clojure.string :refer [split-lines trim]]))

(defn file->str [day part]
  (trim (slurp (format "quest%02d/input%d.txt" day part))))

(defn file->lines [day part]
  (split-lines (file->str day part)))
