(ns advent-of-code.day-06
 (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def file (slurp (io/resource "day-06.txt")))
(defn seperate [file] (str/split file #"\n\n"))
(def example "abc

a
b
c

ab
ac

a
a
a
a

b
")

(defn part-1
  "Day 06 Part 1"
  [input]
  (reduce + (for [group (seperate input)]
              (count (disj (set group) \newline)))))


(defn part-2
  "Day 06 Part 2"
  [input]
  (reduce + (for [group (seperate input)]
              (count (apply set/intersection (for [person (str/split-lines group)]
                                               (set person)))))))


(comment
(part-2 file)
(part-2 example)
)
