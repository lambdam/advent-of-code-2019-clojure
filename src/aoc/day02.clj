(ns aoc.day02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def example "1,9,10,3,2,3,11,0,99,30,40,50")

(-> (io/resource "day02.txt")
    slurp
    #_example
    str/trim
    (str/split #",")
    (->> (mapv #(Integer/parseInt %)))
    (assoc 1 12)
    (assoc 2 2)
    (as-> opcodes'
      (loop [opcodes opcodes'
             pointer* 0]
        (let [v1 (->> pointer* (+ 1) (get opcodes) (get opcodes)) ;; pointer then value
              v2 (->> pointer* (+ 2) (get opcodes) (get opcodes))
              dest* (->> pointer* (+ 3) (get opcodes))]
          (case (get opcodes pointer*)
            1 (recur (assoc opcodes dest* (+ v1 v2))
                     (+ 4 pointer*))
            2 (recur (assoc opcodes dest* (* v1 v2))
                     (+ 4 pointer*))
            99 opcodes
            ;; else
            nil))))
    (get 0))
