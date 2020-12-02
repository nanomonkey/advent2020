(ns advent-of-code.day-02
    (:require [clojure.string :as str]
              [clojure.java.io :refer [resource]]))

(def input (str/split-lines (slurp (resource "day-02.txt"))))

(defn parse [line]
  (re-find #"(\d+)-(\d+) (\S): (\S+)" line))

(defn part-1
  "Day 02 Part 1"
  [input]
  (count (for [x input
               :let [[_ min-str max-str letter password] (parse x)
                     min (Integer/parseInt min-str)
                     max (Integer/parseInt max-str)
                     cnt ((frequencies password) (first (seq letter)))]
               :when (and cnt 
                          (>= cnt min)
                          (<= cnt max))]
           _)))

(defn part-2
  "Day 02 Part 2"
  [input]
  (count (for [x input
               :let [[_ min-str max-str letter-str password] (parse x)
                     letter (first (seq letter-str))
                     min (Integer/parseInt min-str)
                     max (Integer/parseInt max-str)
                     loc1? (= (nth password (- min 1)) letter)
                     loc2? (= (nth password (- max 1)) letter)]
               :when (and (or loc1? loc2?) (not= loc1? loc2?))]
           _)))
