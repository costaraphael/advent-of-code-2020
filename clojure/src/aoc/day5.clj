(ns aoc.day5
  (:require [clojure.string :as string]
            [aoc.util :as util]))

(def input "./../inputs/day5.txt")

(defn- parse-seat-id [seat]
  (->>
   seat
   (map #(if (#{\F \L} %) "0" "1"))
   string/join
   (util/str->int 2)))

(defn- read-input []
  (->>
   input
   (slurp)
   (string/split-lines)
   (map parse-seat-id)))

(defn run-part-1 []
  (apply max (read-input)))

(defn run-part-2 []
  (->>
   (read-input)
   (sort)
   (partition 2 1)
   (filter (fn [[a b]] (= a (- b 2))))
   (map #(+ (first %) 1))
   (first)))