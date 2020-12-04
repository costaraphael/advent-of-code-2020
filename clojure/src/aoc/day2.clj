(ns aoc.day2
  (:require [clojure.string :as string]
            [aoc.util :as util]))

(def input "./../inputs/day2.txt")

(def line-regex #"(?<min>\d+)-(?<max>\d+) (?<char>.): (?<password>.+)")

(defn- parse-line [line]
  (let [matcher (re-matcher line-regex line)]
    (.matches matcher)
    {:password (.group matcher "password")
     :char (->> "char" (.group matcher) first)
     :min (->> "min" (.group matcher) util/str->int)
     :max (->> "max" (.group matcher) util/str->int)}))

(defn- read-input []
  (->>
   input
   (slurp)
   (string/split-lines)
   (map parse-line)))

(defn- part-1-is-valid [{:keys [password char min max]}]
  (->>
   password
   (filter #(= char %))
   (count)
   (#(<= min % max))))

(defn run-part-1 []
  (->>
   (read-input)
   (filter #(part-1-is-valid %))
   (count)))

(defn- part-2-is-valid [{:keys [password char min max]}]
  (->>
   [(get password (dec min)) (get password (dec max))]
   (filter #(= char %))
   (count)
   (#(= 1 %))))

(defn run-part-2 []
  (->>
   (read-input)
   (filter #(part-2-is-valid %))
   (count)))