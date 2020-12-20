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


    ;; Action N means to move the waypoint north by the given value.
    ;; Action S means to move the waypoint south by the given value.
    ;; Action E means to move the waypoint east by the given value.
    ;; Action W means to move the waypoint west by the given value.
    ;; Action L means to rotate the waypoint around the ship left (counter-clockwise) the given number of degrees.
    ;; Action R means to rotate the waypoint around the ship right (clockwise) the given number of degrees.
    ;; Action F means to move forward to the waypoint a number of times equal to the given value.


(defn part-2
  "Day 12 Part 2"
  [input]
  (let [waypoint (atom {:E 10 :N 1})
        n-s (atom 0) 
        e-w (atom 0)]
    (doall (for [[action value] (seperate input)]
             (case action
               "N" (swap! waypoint update-in [:N] #(+ % value))
               "S" (swap! waypoint update-in [:N] #(- % value))
               "E" (swap! waypoint update-in [:E] #(+ % value))
               "W" (swap! waypoint update-in [:E] #(- % value))
               "L" (let [{:keys [E N]} @waypoint] 
                     (case value
                       90 (reset! waypoint {:E (* -1 N) :N E})
                       180 (reset! waypoint {:E (* -1 E) :N (* -1 N)})
                       270 (reset! waypoint {:E N :N (* -1 E)})))
               "R" (let [{:keys [E N]} @waypoint]
                     (case value
                       90 (reset! waypoint {:E N :N (* -1 E)})
                       180 (reset! waypoint {:E (* -1 E) :N (* -1 N)})
                       270 (reset! waypoint {:E (* -1 N) :N E})))
               "F" (let [{:keys [E N]} @waypoint] 
                     (swap! e-w #(+ % (* value E)))
                     (swap! n-s #(+ % (* value N)))))))
    (+ (Math/abs @n-s) (Math/abs @e-w))))

(comment
(part-2 example)
(part-2 file)
)
