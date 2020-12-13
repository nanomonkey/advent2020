(ns advent-of-code.day-09
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.math.combinatorics :as combo]))

(def file (slurp (io/resource "day-09.txt")))

(defn seperate [input] (mapv read-string (str/split-lines input)))

(def example 
"35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576")

(defn valid? [num window]
  ((into #{} (for [[x y] (combo/combinations window 2)] (+ x y))) num))

(defn part-1
  "Day 09 Part 1"
  [input preamble]
  (let [numbers (seperate input)
        position (atom preamble)]
    (while (and (< @position (count numbers)) 
                (valid? (nth numbers @position) 
                        (take preamble (drop (- @position preamble) numbers))))
      (swap! position inc))
    (nth numbers @position)))

(valid? 3 '(1 2))

(comment 
(part-1 example 5)
(part-1 file 25)
)


(defn part-2 
  "find a contiguous set of at least two numbers in your list which sum to 32321523"
  [input check-num]
  (let [numbers (seperate input)]
    (loop [position 0
           width 2]
      (let [window (take width (drop position numbers))
            result (apply + window)]
        (cond 
          (< result check-num) (recur position (inc width))
          (> result check-num) (recur (inc position) 2)
          :else  (+ (apply min window) (apply max window)))))))

(comment
(part-2 example 127)
(part-2 file 32321523)
)
