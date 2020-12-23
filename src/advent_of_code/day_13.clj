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

; ********************* Part 2 ******************

(defn parse-2 [input]
  (let [[_ buses] (str/split-lines input)] 
    (str/split buses #",")))

(defn remove-x-keep-index [buses]
  (vec (doall (for [bus buses
                    :when (not= bus "x")]
                [(read-string bus) (.indexOf buses bus)]))))

(defn gcd [x y]
  "greatest common divisor"
  (let [a (mod x y)]
    (if (= 0 a)
      y
      (gcd y a))))

(defn lcm [x y]
  "least common multiple"
  (/ (* x y) (gcd x y)))

(defn part-2-old
  "Day 13 Part 2"
  [input]
  (let [buses (remove-x-keep-index (parse-2 input))
        period (ffirst buses)]
    (loop [t period]
      (if (zero? 
           (reduce + 
                   (map 
                    (fn [[moduli indx]] (mod (+ t indx) moduli)) 
                    (rest buses))))
        t
        (recur (+ t period))))))

(defn part-2
  "Day 13 Part 2"
  [input]
  (let [buses (remove-x-keep-index (parse-2 input))
        period (ffirst buses)]
    (loop [t period
           step period
           i 1] 
      (if-let [[moduli indx] (get buses i)]
        (if (zero? (mod (+ t indx) moduli))    
          (recur t (lcm step moduli) (inc i))
          (recur (+ t step) step  i))
        t))))

(comment         
  (part-2 example) ;;   1068781
  (part-2 file)
)
