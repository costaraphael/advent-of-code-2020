(ns aoc.day8
  (:require [clojure.string :as string]
            [aoc.util :as util]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

(def input "./../inputs/day8.txt")

(defn- parse-instruction [instruction]
  (let [[command arg] (string/split instruction #" ")]
    [command (util/str->int arg)]))

(defn- read-input []
  (with-open [rdr (io/reader input)]
    (->>
     rdr
     line-seq
     (map parse-instruction)
     (vec))))

(defn- execute [program]
  (loop [idx 0 acc 0 seen #{}]
    (cond
      (seen idx) {:infinite-loop acc}
      (<= (count program) idx) {:finished acc}
      :else (let [seen (conj seen idx)]
              (match (get program idx)
                ["nop" arg] (recur (inc idx) acc seen)
                ["acc" arg] (recur (inc idx) (+ acc arg) seen)
                ["jmp" arg] (recur (+ idx arg) acc seen))))))

(defn run-part-1 []
  (let [program (read-input)
        {acc :infinite-loop} (execute program)]
    acc))

(defn- find-jmps-and-nops [program]
  (->>
   program
   (map vector (iterate inc 0))
   (filter #(->> % (second) (first) #{"jmp" "nop"}))
   (map first)))

(defn- swap-instruction [program idx]
  (match (get program idx)
    ["nop" arg] (assoc program idx ["jmp" arg])
    ["jmp" arg] (assoc program idx ["nop" arg])))

(defn run-part-2 []
  (let [program (read-input)
        {acc :finished} (->>
                         program
                         (find-jmps-and-nops)
                         (map #(->> % (swap-instruction program) (execute)))
                         (filter :finished)
                         (first))]
    acc))
