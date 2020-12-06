(ns advent-of-code.day-05
   (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def file (slurp (io/resource "day-05.txt")))
(defn seperate [file] (str/split-lines file))

(def example "BFFFBBFRRR
FFFBBBFRRR
BBFFBBFRLL") ;(567 119 820)

(defn seat-id [row column] (+ (* row 8) column))

(defn ->bits [str] (str/replace (str/replace str #"[FL]" "0") #"[BR]" "1"))

(defn seats [input] (for [x (seperate input)
                          :let [bits (->bits x)]]
                      (seat-id (read-string (str "2R" (subs bits 0 7))) 
                               (read-string (str "2R" (subs bits 7 10))))))

(defn part-1
  "Day 05 Part 1"
  [input]
  (apply max (seats input)))

(comment
(part-1 example)
(part-1 file)
)

(defn part-2
  "Day 05 Part 2"
  [input]
  (set/difference (apply sorted-set (range 871)) 
                  (apply sorted-set (seats input))))

(comment
(part-2 file)
)
