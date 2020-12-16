(ns advent-of-code.day-10
  (:require [clojure.java.io :as io]))

(def file (slurp (io/resource "day-10.txt")))

(defn seperate [input] (into [] (map read-string (str/split-lines input))))

(def example "16
10
15
5
1
11
7
19
6
12
4")

(defn part-1
  "Day 10 Part 1"
  [input]
  (let [three-jolt (atom 1) ;;start with one extra for the last adapter
        one-jolt (atom 0)]
    (doseq [[a b] (partition 2 1 (sort (conj (seperate input) 0)))]
      (case (- b a)
        3 (swap! three-jolt inc)
        1 (swap! one-jolt inc)))
    (* @one-jolt @three-jolt)))



(def break-at-3
  (memoize
   (fn
     ([fst] 1)
     ([fst scd & rst]ma
      (if (> (+ fst scd) 3)
        (apply break-at-3 scd rst)
        (+ (apply break-at-3 scd rst)
           (apply break-at-3 (+ fst scd) rst)))))))

(defn part-2
  "Day 10 Part 2"
  [input]
  (let [deltas (map (fn [[a b]] (- b a)) (partition 2 1 (sort (conj (seperate input) 0))))]
    (apply break-at-3 deltas)))

 
(comment
(part-2 example)
(part-2 file)
)
