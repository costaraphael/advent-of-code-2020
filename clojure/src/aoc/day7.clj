(ns aoc.day7
  (:require [aoc.util :as util]
            [clojure.java.io :as io]))

(def input "./../inputs/day7.txt")

(defn- parse-line [line]
  (let [[[_, _, outer-color] & parts] (re-seq #"(?:^|(\d+) )(\w+ \w+) bags?" line)
        inner-map (->>
                   parts
                   (map (fn [[_ amount color]] [color (util/str->int amount)]))
                   (into {}))]
    [outer-color inner-map]))

(defn- read-input []
  (with-open [rdr (io/reader input)]
    (->>
     rdr
     line-seq
     (map parse-line)
     (into {}))))

(defn- container-bags [color bags-map]
  (->>
   bags-map
   (map (fn [[container-color inner-map]]
          (if (inner-map color)
            (conj (container-bags container-color bags-map) container-color)
            [])))
   (flatten)))

(defn run-part-1 []
  (->>
   (read-input)
   (container-bags "shiny gold")
   (distinct)
   (count)))

(defn- inner-bags-amount [color bags-map]
  (->>
   color
   bags-map
   (map (fn [[inner-color amount]]
          (+ amount (* amount (inner-bags-amount inner-color bags-map)))))
   (apply +)))

(defn run-part-2 []
  (->>
   (read-input)
   (inner-bags-amount "shiny gold")))