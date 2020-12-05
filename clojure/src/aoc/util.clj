(ns aoc.util)

(defn spy
  ([value]
   (println "DEBUG:" value)
   value)

  ([prompt value]
   (println prompt ":" value)
   value))

(defn tee [f coll]
  (map #(do (f %) %) coll))

(defn str->int [str] (Integer/parseInt str))

(defn update-existing [map key f & args]
  (cond
    (key map) (update map key #(apply f (conj args %)))
    :else map))
