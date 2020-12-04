(ns advent-of-code.day-03
  (:require [clojure.string :as str]
            [clojure.java.io :as io]))

(def file (slurp (io/resource "day-03.txt")))

(def example "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#")

(defn- read-lines [file]
  (with-open [rdr (io/reader file)]
    (doall (line-seq rdr))))

(defn tree? [line column]
  (if (= \# (nth line (mod column (count line))))
    1 0))

(defn part-1
  "Day 03 Part 1"
  [input]
  (reduce +
          (for [x (range (count input))]
            (tree? (get input x) (* x 3)))))

(defn part-2
  "Day 03 Part 2"
  [input]
  (apply *
         (for [slope [{:r 1 :d 1} {:r 3 :d 1} {:r 5 :d 1} {:r 7 :d 1} {:r 1 :d 2}]]
           (reduce +
                   (let [cnt (count input)]
                     (for [i (range cnt)
                           :let [x (* i (slope :r))
                                 y (* i (slope :d))]
                           :when (<= y cnt)]
                       (tree? (get input y) x)))))))



(comment 
 (count (str/split-lines example))
  (part-2 (str/split-lines example))
  (part-2 (str/split-lines file))
)
