(ns advent-of-code.day-16
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]))

(def file (slurp (io/resource "day-16.txt")))

(def re-rules #"([\s\w]*): (\d+)-(\d+) or (\d+)-(\d+)")

(defn sections [input]
  (let [[rules your-ticket nearby-tickets] (str/split  input #"\n\n\w+ tickets*:\n")]
    {:rules (vec (str/split-lines rules))
     :your-ticket (mapv read-string (str/split your-ticket #","))
     :nearby-tickets (mapv #(str/split % #",")
                              (str/split-lines nearby-tickets))}))

(defn rule-set [rules]
  (into #{} (apply concat
             (for [rule rules
                   :let [[_ name start1 end1 start2 end2] (re-find re-rules rule)]]
               (concat (range (read-string start1) (read-string end1)) 
                       (range (read-string start2) (read-string end2)))))))

(defn part-1
  "Day 16 Part 1"
  [input]
  (let [{:keys [rules your-ticket nearby-tickets]} (sections input)
        rule-set (rule-set rules)]
    (reduce + (apply concat
                     (for [ticket nearby-tickets]
                       (filter #(not (contains? rule-set %)) (map read-string ticket)))))))


(comment
  (part-1 file)
)

;*************** Part 2 ***********************

(defn part-2
  "Day 16 Part 2"
  [input]
  input)
