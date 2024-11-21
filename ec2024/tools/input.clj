(ns tools.input
  (:require [clojure.string :refer [split-lines trim]]))

(defn file->str [file]
  (let [quest (last (re-find #"(quest\d+)" (str *ns*)))
        dir (or quest ".")]
    (trim (slurp (format "%s/%s" dir file)))))

(defn file->lines [file]
  (split-lines (file->str file)))
