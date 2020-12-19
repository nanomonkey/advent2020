(ns advent-of-code.day-12
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def file (slurp (io/resource "day-12.txt")))

(defn parse [line]
  (let [[_ action value] (re-find #"([NSEWFLR])(\d+)" line)]
    [action (read-string value)]))

(defn seperate [input] (map parse (str/split-lines input)))

(def example "F10
N3
F7
R90
F11")

(defn left [direction degrees]
  (let [index (str/index-of "NWSE" direction)]
    (nth (cycle ["N" "W" "S" "E"]) (+ index (/ degrees 90)))))

(defn right [direction degrees]
  (let [index (str/index-of "NESW" direction)]
    (nth (cycle ["N" "E" "S" "W"]) (+ index (/ degrees 90)))))

(defn part-1
  "Day 12 Part 1"
  [input]
  (let [facing (atom "E")
        n-s (atom 0)
        e-w (atom 0)]
    (doall
     (for [[action value] (seperate input)]
       (case action
         "N" (swap! n-s #(+ % value))
         "S" (swap! n-s #(- % value))
         "E" (swap! e-w #(+ % value))
         "W" (swap! e-w #(- % value))
         "F" (case @facing
               "N" (swap! n-s #(+ % value))
               "S" (swap! n-s #(- % value))
               "E" (swap! e-w #(+ % value))
               "W" (swap! e-w #(- % value)))
         "L" (swap! facing #(left % value))
         "R" (swap! facing #(right % value)))))
    (+ (Math/abs @n-s) (Math/abs @e-w))))

(comment
(part-1 example)
(part-1 file)
)

; ************ Part 2 ************************

(defn part-2
  "Day 12 Part 2"
  [input]
  (let [waypoint {:E 10 :N 1}
        facing "E"])
  (doall (for [[action value] (seperate input)]
           )))

;10 units east and 1 unit north
