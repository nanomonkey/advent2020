(ns advent-of-code.day-11
  (:require [clojure.java.io :as io]))

(def file (slurp (io/resource "day-11.txt")))

(defn seperate [input] (mapv #(str/split % #"") (str/split-lines input)))

(def example "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL")

(defn seat [[x y] layout]
  (get-in layout [y x]))

(defn occupied? [[x y] layout]
  (if (= "#" (seat [x y] layout)) 1 0))

(defn occupied-adjacent [[x y] layout]
  (reduce + (for [i [-1 0 1] ;; no need to worry about being outside of
                    j [-1 0 1] ;; layout due to nil punting
                    :when (not= [i j] [0 0])] ;; check seat itself??
              (occupied? [(+ i x) (+ j y)] layout))))


(defn check-n-sit! [[x y] layout]
  (case (seat [x y] layout)
    "." "."
    "#" (if (>= (occupied-adjacent [x y] layout) 4) "L" "#")
    "L" (if (= 0 (occupied-adjacent [x y] layout)) "#" "L")))

(defn update-layout! [layout]
  (vec (for [y (range (count layout))]
         (vec (for [x (range (count (get layout 0)))] (check-n-sit! [x y] layout))))))

(defn count-occupied [layout]
  (reduce + (for [x (range (count (get layout 0)))
               y (range (count layout))]
              (occupied? [x y] layout))))

(defn part-1
  "Day 11 Part 1"
  [input]
  (loop [seat-layout (seperate input)
         cnt 0]
    ;(println cnt)
    (let [new-layout (update-layout! seat-layout)]
      ;(println new-layout)
      (if (= new-layout seat-layout)
        (count-occupied new-layout)
        (recur new-layout (inc cnt))))))

(comment
(part-1 example)
(part-1 file)
)

;;***************** Part 2 *******************************

(defn look? [[x y] [i j] layout]
  (loop [new-seat [(+ i x) (+ j y)]]
    (case (seat new-seat layout)
      "L" 0
      "#" 1
      "." (recur (map + new-seat [i j]))
      nil 0)))

(defn occupied-visible [[x y] layout]
  (reduce + (for [i [-1 0 1] ;; no need to worry about being outside of
                  j [-1 0 1] ;; layout due to nil punting
                  :when (not= [i j] [0 0])] ;; no need to check seat itself
              (look? [x y] [i j] layout))))

(defn check-visible-n-sit! [[x y] layout]
  (case (seat [x y] layout)
    "." "."
    "#" (if (>= (occupied-visible [x y] layout) 5) "L" "#")
    "L" (if (= 0 (occupied-visible [x y] layout)) "#" "L")))

(defn update-layout2! [layout]
  (vec (for [y (range (count layout))]
         (vec (for [x (range (count (get layout 0)))] (check-visible-n-sit! [x y] layout))))))

(defn part-2
  "Day 11 Part 2"
  [input]
  (loop [seat-layout (seperate input)]
    (let [new-layout (update-layout2! seat-layout)]
      (if (= new-layout seat-layout)
        (count-occupied new-layout)
        (recur new-layout)))))



(comment
(part-2 example)
(part-2 file)
)
