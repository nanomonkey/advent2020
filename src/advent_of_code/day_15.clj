(ns advent-of-code.day-15
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]))

(def puzzle-input "14,1,17,0,3,20")
(def example "0,3,6")

(defn last-seen [x nums]
  (let [seen 
        (first (for [[num indx] (partition 2 (interleave (reverse (butlast nums)) (range)))
                     :when (= num x)]
                 (+ 1 indx)))]
    (if (nil? seen) 0 seen)))

(defn part-1
  "Day 15 Part 1"
  [input end]
  (loop [nums (into [] (map read-string (str/split input #",")))
         turn (count nums)]
    ;(println turn nums)
    (let [recent (last nums)]
      (if (= turn end) 
        recent
        (recur (conj nums (last-seen recent nums)) (inc turn))))))

(comment
(part-1 example 10)
(part-1 example 2020)
(part-1 puzzle-input 2020)
)

;************ Part 2 **********************

(defn parse [input] 
  (into {}
        (for [[k v]
              (partition 2 (interleave (map read-string (str/split input #",")) 
                                       (range 1 (+ 1 (count input)))))]
          {k v})))

(defn part-2
  "Day 15 Part 2"
  [input end]
  (loop [last-index (parse input)
         turn (count last-index)
         last-val (key (apply max-key val last-index))]
    ;(println turn last-index last-val)
    (if (= turn end) last-val
        (let [new-val (- turn (get last-index last-val turn))]
          (recur (conj last-index {last-val turn}) 
                 (inc turn) 
                 new-val)))))


(comment 
(part-2 example 10)
(assert (= 1 (part-2 "1,3,2" 2020)))
(part-2 example 2020)
(part-2 puzzle-input 30000000)
)
