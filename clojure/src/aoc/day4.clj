(ns aoc.day4
  (:require [clojure.string :as string]
            [clojure.spec.alpha :as spec]
            [aoc.util :as util]))

(def input "./../inputs/day4.txt")

(defn- parse-passport [passport-input]
  (as->
   passport-input $
    (string/join " " $)
    (string/split $ #" ")
    (map (fn [part]
           (let [[field value] (string/split part #":")]
             [(keyword field) value])) $)
    (into {} $)
    (util/update-existing $ :byr util/str->int)
    (util/update-existing $ :iyr util/str->int)
    (util/update-existing $ :eyr util/str->int)))

(defn read-input []
  (->>
   input
   slurp
   string/split-lines
   (partition-by #(= % ""))
   (filter #(not= % '("")))
   (map parse-passport)))

(spec/def ::part-1-passport (spec/keys :req-un [:part-1/byr :part-1/iyr :part-1/eyr :part-1/hgt :part-1/hcl :part-1/ecl :part-1/pid]
                                       :opt-un [:part-1/cid]))

(defn run-part-1 []
  (->>
   (read-input)
   (filter #(spec/valid? ::part-1-passport %))
   (count)))

(defn- format-matches [re]
  #(re-matches re %))

(defn valid-height? [height]
  (let [regex #"(\d+)(in|cm)"
        [_ number unit] (re-matches regex height)
        number (and number (util/str->int number))]
    (or
     (and (= unit "in") (<= 59 number 76))
     (and (= unit "cm") (<= 150 number 193)))))

(defn always-true [_] true)

(spec/def :part-2/byr (spec/int-in 1920 2003))
(spec/def :part-2/iyr (spec/int-in 2010 2021))
(spec/def :part-2/eyr (spec/int-in 2020 2031))
(spec/def :part-2/hgt valid-height?)
(spec/def :part-2/hcl (format-matches #"#[0-9a-f]{6}"))
(spec/def :part-2/ecl #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})
(spec/def :part-2/pid (format-matches #"\d{9}"))

(spec/def ::part-2-passport (spec/keys :req-un [:part-2/byr :part-2/iyr :part-2/eyr :part-2/hgt :part-2/hcl :part-2/ecl :part-2/pid]
                                       :opt-un [:part-2/cid]))

(defn run-part-2 []
  (->>
   (read-input)
   (filter #(spec/valid? ::part-2-passport %))
   (count)))