(ns aoc.util)

(defn spy [value]
  (println "DEBUG:" value)
  value)

(defn str->int [str] (Integer/parseInt str))
