(ns aoc.day3
  (:require [clojure.string :as string]))

(def input "./../inputs/day3.txt")

(defprotocol ITreeMap
  (is-out-of-bounds [this y])
  (get-item [this x y]))

(defrecord TreeMap [contents height width]
  ITreeMap
  (is-out-of-bounds [this y] (<= height y))
  (get-item [this x y]
    (let [actual-x (mod x width)]
      (->
       contents
       (get y)
       (get actual-x)))))

(defn- parse-tree-map-contents [str]
  (->>
   str
   (string/split-lines)
   (map (fn [line] (->> line (map #(if (= % \#) :tree :empty)) vec)))
   vec))

(defn- read-input []
  (let [contents (->> input (slurp) parse-tree-map-contents)
        height (count contents)
        width (-> contents (get 0) count)]
    (TreeMap. contents height width)))

(defn- iterate-tree-map [x-movement y-movement tree-map]
  (letfn [(iterate-tree-map* [x y]
            (if (.is-out-of-bounds tree-map y)
              ()
              (lazy-seq (cons (.get-item tree-map x y) (iterate-tree-map* (+ x x-movement) (+ y y-movement))))))]
    (iterate-tree-map* 0 0)))

(defn run-part-1 []
  (->>
   (read-input)
   (iterate-tree-map 3 1)
   (filter #(= :tree %))
   (count)))

(defn run-part-2 []
  (let [tree-map (read-input)]
    (->>
     [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]]
     (map (fn [[x-movement y-movement]]
            (->>
             tree-map
             (iterate-tree-map x-movement y-movement)
             (filter #(= :tree %))
             (count))))
     (apply *))))