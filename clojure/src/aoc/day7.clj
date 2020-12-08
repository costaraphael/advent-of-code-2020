(ns aoc.day7
  (:require [clojure.string :as string]
            [aoc.util :as util]))

(def input "./../inputs/day7.txt")

(defn- parse-part [part]
  (let [[amount & rest] (string/split part #" ")
        amount (util/str->int amount)
        color (->> rest (remove #{"bag" "bags"}) (string/join " "))]
    [color amount]))

(defn- parse-line [line]
  (let [[outer_color parts] (string/split line #" bags contain ")]
    (cond
      (= parts "no other bags") [outer_color {}]
      :else [outer_color (->>
                          (string/split parts #", ")
                          (map parse-part)
                          (into {}))])))

(defn- read-input []
  (as->
   input $
    (slurp $)
    (string/split $ #".\n")
    (map parse-line $)
    (into {} $)))

(defn- container-bags [color bags-map]
  (->>
   bags-map
   (map (fn [[container-color inner-map]]

          (if (get inner-map color)
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
   (get bags-map)
   (map (fn [[inner-color amount]]
          (+ amount (* amount (inner-bags-amount inner-color bags-map)))))
   (apply +)))

(defn run-part-2 []
  (->>
   (read-input)
   (inner-bags-amount "shiny gold")))