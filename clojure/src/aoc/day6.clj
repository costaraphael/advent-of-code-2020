(ns aoc.day6
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def input "./../inputs/day6.txt")

(defn- read-input []
  (->>
   input
   (slurp)
   (string/split-lines)
   (partition-by #(= % ""))
   (filter #(not= % '("")))
   (map #(map set %))))

(defn run-part-1 []
  (->>
   (read-input)
   (map #(apply set/union %))
   (map count)
   (apply +)))

(defn run-part-2 []
  (->>
   (read-input)
   (map #(apply set/intersection %))
   (map count)
   (apply +)))