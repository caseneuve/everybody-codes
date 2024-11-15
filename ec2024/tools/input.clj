(ns tools.input
  (:require [clojure.string :refer [trim]]))

(defn file->str [day part]
  (trim (slurp (format "quest%02d/input%d.txt" day part))))
