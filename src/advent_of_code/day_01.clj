(ns advent-of-code.day-01
  (:require [clojure.string :as str]
            [clojure.java.io :refer [resource]])) 

(def input (map #(Integer/parseInt %) (str/split-lines (slurp (resource "day-01.txt")))))

(defn part-1
  [input]
  (for [x input
          y input
          :let [z (+ x y)]
          :when (and (not= x y) (= 2020 z))]
      (* x y)))

(defn part-2
  "Day 01 Part 2"
  [input]
  (for [x input
        y input
        z input
        :let [w (+ x y z)]
        :when (and (not= x y z) (= 2020 w))]
    (* x y z)))
