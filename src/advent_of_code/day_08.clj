(ns advent-of-code.day-08
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]))

(def file (slurp (io/resource "day-08.txt")))

(defn get-instructions [input] (for [line (str/split-lines input) 
                                     :let [[operation arg] (str/split line #" ")]]
                                 [operation (read-string arg)]))

(def example "nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6")

(-> example
    get-instructions)

(defn part-1
  "Day 08 Part 1"
  [input]
  (let [instructions (get-instructions input)
        accumulator (atom 0)
        position (atom 0)
        visited (atom #{})]
    (while (not (@visited @position))
      (do (swap! visited conj @position)
          (let [[operation arg] (nth instructions @position)]
            (case operation
              "acc" (do (swap! accumulator #(+ arg %))
                        (swap! position inc))
              "nop" (swap! position inc)
              "jmp" (swap! position #(+ arg %))))))
    @accumulator))

(comment
(part-1 example)
(part-1 file)
)

(defn part-2
  "Day 08 Part 2"
  [input]
  (let [instructions (into [] (get-instructions input))
        instruct-cnt (count instructions)]
    (filter some? (for [pos (range  instruct-cnt)
                        :when (not= "acc" (first (nth instructions pos)))
                        :let [[inst arg] (nth instructions pos)
                              new-instr (assoc instructions pos 
                                               [(if (= inst "jmp") "nop" "jmp") arg])]]
                    (let [accumulator (atom 0)
                          position (atom 0)
                          visited (atom #{})]
                      (while (and (< @position instruct-cnt) (not (@visited @position)))
                        (do (swap! visited conj @position)
                            (let [[operation arg] (nth new-instr @position)]
                              (case operation
                                "acc" (do (swap! accumulator #(+ arg %))
                                          (swap! position inc))
                                "nop" (swap! position inc)
                                "jmp" (swap! position #(+ arg %))))))
                      (when (>= @position instruct-cnt) @accumulator))))))

(comment
  (part-2 example)
  (part-2 file)
)

