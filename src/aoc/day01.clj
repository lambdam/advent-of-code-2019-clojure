(ns aoc.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn part1 [weight]
  (-> weight (quot 3) (- 2)))

(defn part2 [weight']
  (loop [weight weight'
         acc 0]
    (let [fuel (-> weight (quot 3) (- 2))]
      (if (> fuel 0)
        (recur fuel (+ acc fuel))
        acc))))

(-> (io/resource "day01.txt")
    slurp
    str/trim
    (str/split #"\s+")
    (->> (map #(Integer/parseInt %)))
    (->> (map part1))
    ;; (->> (map part2))
    (->> (apply +)))
