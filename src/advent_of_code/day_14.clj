(ns advent-of-code.day-14
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]))

(def file (slurp (io/resource "day-14.txt")))

(defn split-program [input]
  (rest (str/split input #"mask = ")))

(def example "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0")

(def re-mem #"mem\[([0-9]*)\] = ([0-9]*)")

(defn parse [input] 
  (let [lines (str/split-lines input)
        mask (first lines)
        mems  (for [line (rest lines) 
                    :let [[_ address value] (re-find re-mem  line)]]
                [(read-string address) (read-string value)])]
    {:mask mask
     :mems mems}))

(defn apply-mask [mask mem]
  (let [result (atom mem)]
     (doseq [[m indx] 
             (partition 2 (interleave (reverse (str/split mask #"")) (range)))
             :when (not= m "X")]
         (case m
           "1" (swap! result #(bit-set % indx))
           "0" (swap! result #(bit-clear % indx))))
    @result))

(defn part-1
  "Day 14 Part 1"
  [input]
  (let [mem-addrs (atom {})
        programs (map parse (split-program input))]
    (doseq [{:keys [mask mems]} programs]
      (doseq [[address value] mems]
        (swap! mem-addrs conj {address (apply-mask mask value)})))
    (reduce + (vals @mem-addrs))))

(comment
  (part-1 example)
  (part-1 file)
)

;***************** Part 2 ********************

(defn mem-addr-decoder [mask mem]
  (let [base (atom mem)
        offsets (atom [])]
    (doseq [[m indx]
            (partition 2 (interleave (reverse (str/split mask #"")) (range)))
            :when (not= m "0")]
      (case m
        "1" (swap! base #(bit-set % indx)) 
        "X" (do (swap! base #(bit-clear % indx)) 
                (swap! offsets #(conj % (bit-set 0 indx))))))
    (map #(apply + @base %) (combo/subsets @offsets))))

(mem-addr-decoder "000000000000000000000000000000X1001X" 42)

(defn part-2
  "Day 14 Part 2"
  [input]
  (let [mem-addrs (atom {})
        programs (map parse (split-program input))]
    (doseq [{:keys [mask mems]} programs]
      (doseq [[address value] mems]
        (doseq [mem (mem-addr-decoder mask address)]
          (swap! mem-addrs conj {mem value}))))
    (reduce + (vals @mem-addrs))))

(def example2 "mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1")

(comment
(part-2 example2)
(print (part-2 file))
)
