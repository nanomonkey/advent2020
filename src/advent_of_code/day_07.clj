(ns advent-of-code.day-07
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]
            [clojure.set :as set]))

(def file (slurp (io/resource "day-07.txt")))

(defn seperate [file] (str/split file #"\.\n"))

(def example "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")

(defn parse-contains [contains] 
  (into {}
        (for [item (str/split contains #", ")
              :let [[_ num color] (re-find  #"(\d+|no) (\w* \w*) bag" item)]]
          {color num})))

(defn parse-description [description]
  (let [[color contains] (str/split description #" bags contain ")]
       {color (parse-contains contains)}))

(defn bags-that-hold [color map]
  (let [bags (atom #{})]
    (doall
     (for [[k v] map
           :when (and (contains? v color)
                      (not= {nil nil} (get-in v [color :num])))]
       (when (not (contains? @bags k)) 
         (do (swap! bags conj k)
             (doseq [x (bags-that-hold k map)] (swap! bags conj x) )))))
    @bags))

(defn part-1 [input]
  "How many bag colors can eventually contain at least one shiny gold bag?"
  (let [bag-map (into {} (for [description (seperate input)]
                         (parse-description description)))] 
    (count (bags-that-hold "shiny gold" bag-map))))


(defn count-bags [color tree]
  (let [accum
        (reduce (fn [cnt [bag num]] 
                  (if (string? num) 
                    (+ cnt (* (read-string num) (count-bags bag tree)))
                    1))
                1
                (tree color))]
    accum))

(defn part-2 [input]
  "How many individual bags are required inside your single shiny gold bag?"
  (let [bag-dependencies (into {} (for [description (seperate input)]
                                    (parse-description description)))] 
    (dec (count-bags "shiny gold" bag-dependencies))))

(def example2 "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.")

(comment
 (part-2 example) ;; 32
(part-2 example2) ;; 126
(part-2 file)
)
