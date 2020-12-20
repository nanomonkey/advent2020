(ns advent-of-code.day-13
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def file (slurp (io/resource "day-13.txt")))

(defn parse [input]
  (let [[earliest buses] (str/split-lines input)]
    [(read-string earliest) 
     (map read-string (filter #(not= "x" %) (str/split buses #",")))]))

(def example "939
7,13,x,x,59,x,31,19")

(let [[earliest buses] (-> example parse)
      bus (first buses)]
  (- bus (mod 939 bus)))

(defn part-1
  "Day 13 Part 1"
  [input]
  (let [[earliest buses] (parse input)]
    (apply *
           (apply min-key second
                  (for [bus buses]
                    [bus (- bus (mod earliest bus))])))))

(comment
(part-1 example)
(part-1 file)
)
(defn part-2
  "Day 13 Part 2"
  [input]
  input)
