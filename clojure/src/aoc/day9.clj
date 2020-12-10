(ns aoc.day9
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(def input "./../inputs/day9.txt")

(defn- read-input []
  (with-open [rdr (io/reader input)]
    (->>
     rdr
     line-seq
     (map bigint)
     (vec))))

(defn- find-bad-number [numbers]
  (let [[preamble numbers] (split-at 25 numbers)]
    (loop [preamble preamble
           [number & numbers] numbers]
      (if number
        (as-> 2 $
          (combo/combinations preamble 2)
          (some (fn [[a b]] (= (+ a b) number)) $)
          (if $
            (recur (concat (rest preamble) [number]) numbers)
            number))
        nil))))

(defn run-part-1 []
  (->>
   (read-input)
   (find-bad-number)))

(defn- find-bad-number-sum [numbers bad-number]
  (letfn [(find-sum-seq* [numbers]
            (loop [[number & numbers] numbers
                   acc 0
                   seq []]
              (cond
                (and (= acc bad-number) (= (count seq) 1)) nil
                (= acc bad-number) seq
                (> acc bad-number) nil
                :else (recur numbers (+ acc number) (conj seq number)))))]
    (loop [numbers numbers]
      (or (find-sum-seq* numbers) (recur (rest numbers))))))

(defn run-part-2 []
  (let [numbers (read-input)]
    (->>
     numbers
     (find-bad-number)
     (find-bad-number-sum numbers)
     (apply (juxt min max))
     (apply +))))
