(ns hafni-seesaw.utils
  (:require [clojure.tools.logging :as log]))

(defn rmap [f coll]
  (if (coll? coll)
    (map #(rmap f %) coll)
    (f coll)))

(defn find-i
  "Find the index of value in coll"
  [value coll]
  (let [limit (count coll)]
    (loop [i 0
           c coll]
      (if (== i limit)
        nil
        (if (= (first c) value)
            i
            (recur (inc i) (rest c)))))))

(defn replace-i
  "change index n in coll to value"
  [n value coll]
  (concat (take n coll)
          [value]
          (drop (inc n) coll)))

(defn change-i
  "change index n in coll with f"
  [n f coll]
  (replace-i n (f (nth coll n)) coll))

(defn drop-nth [n coll]
  (concat (take n coll)
          (rest (drop n coll))))

(defn ignore 
  "Takes the function f of 0 args and creates a
function with any number of args which are ignored."
  [f] (fn [& _] (f)))

(defn throw-field-exist [c field]
  (throw (Exception. (str "The field " field " does not exist for object " c))))

