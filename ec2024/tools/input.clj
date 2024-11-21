(ns tools.input
  (:require [clojure.string :refer [split-lines trim]]))

(defn file->str [part file]
  (trim (slurp (format "quest%02d/%s" (Integer. part) file))))

(defn file->lines [part file]
  (split-lines (file->str part file)))
