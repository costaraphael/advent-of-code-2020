(ns aoc.day1
  (:require [clojure.string :as string]
            [aoc.util :as util]
            [clojure.math.combinatorics :as combo]))

(def input "./../inputs/day1.txt")

(defn read-input []
  (->>
   input
   (slurp)
   (string/split-lines)
   (map util/str->int)))

(defn run-part-1 []
  (->>
   (read-input)
   (#(combo/combinations % 2))
   (filter #(= 2020 (apply + %)))
   (first)
   (apply *)))

(defn run-part-2 []
  (->>
   (read-input)
   (#(combo/combinations % 3))
   (filter #(= 2020 (apply + %)))
   (first)
   (apply *)))