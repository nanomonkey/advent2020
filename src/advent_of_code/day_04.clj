(ns advent-of-code.day-04
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.edn :as edn]))

(def file (slurp (io/resource "day-04.txt")))
(def fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]) ;;"cid" removed

(def example "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in")

(defn seperate [file] (str/split file #"\n\n"))
(defn ->kv [acct] (into (sorted-map) (map #(str/split % #":") (str/split acct #"\s"))))
(defn has-fields? [acct-map] (every? acct-map fields))

(defn part-1
  "Day 04 Part 1"
  [input]
  (reduce +
          (for [acct (seperate input)
                :when (has-fields? (->kv acct))]
            1)))


(def invalid-passports "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926

iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946

hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277

hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007")

(def valid-passports "pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980
hcl:#623a2f

eyr:2029 ecl:blu cid:129 byr:1989
iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm

hcl:#888785
hgt:164cm byr:2001 iyr:2015 cid:88
pid:545766238 ecl:hzl
eyr:2022

iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719")

(s/def ::expr (s/cat :lhs ::lhs :arrow #{'->} :rhs ::rhs))

(defn parsed-lines [lines]
  (mapv (fn [l]
          (let [edn (edn/read-string
                     (format "[%s]" l))]
            (s/conform ::expr edn)))
        lines))


(defn byr? [byr] (try (<= 1920 (Long/parseLong byr) 2002)
                      (catch Exception e false)))

(defn iyr? [iyr] (try (<= 2010 (Long/parseLong iyr) 2020)
                      (catch Exception e false)))

(defn eyr? [eyr] (try (<= 2020 (Long/parseLong eyr) 2030)
                      (catch Exception e false)))

(defn hgt? [hgt] (let [[_ num unit] (re-find #"(\d*)(\S+)" hgt)]
                   (try (case unit
                          "in" (<= 59 (Long/parseLong num) 76)
                          "cm" (<= 150 (Long/parseLong num) 193)
                          false)
                        (catch Exception e false))))

(defn hcl? [hcl] (let [[_ hex] (re-find #"#([0-9a-f]{6})" hcl)] (some? hex)))

(defn ecl? [ecl] (some #(= ecl %) ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"]))

(defn pid? [pid] (and (= 9 (count pid)) 
                      (let [[_ id] (re-find #"([0-9]{9})" pid)] (some? id))))

(defn valid? [passport] (and (has-fields? passport)
                               (byr? (passport "byr"))
                               (iyr? (passport "iyr"))
                               (eyr? (passport "eyr"))
                               (hgt? (passport "hgt"))
                               (hcl? (passport "hcl"))
                               (ecl? (passport "ecl"))
                               (pid? (passport "pid"))))

(defn part-2
  "Day 04 Part 2"
  [input]
  (reduce + 
          (for [acct (seperate input)
                :let [passport (->kv acct)]
                :when (valid? passport)]
             1)))


(comment 
 (part-2 valid-passports)
 (part-2 invalid-passports)
 (part-2 example)
 (part-2 file)
)
