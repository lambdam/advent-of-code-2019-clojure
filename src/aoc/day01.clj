(ns aoc.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.alpha.spec :as s]))

(s/def ::weight integer?)

(s/def ::fuel integer?)

(defn part1 [weight]
  (-> weight (quot 3) (- 2)))

(s/fdef part1
  :args (s/cat :weight ::weight)
  :ret ::fuel)

(defn part2 [weight']
  (loop [weight weight'
         acc 0]
    (let [fuel (-> weight (quot 3) (- 2))]
      (if (> fuel 0)
        (recur fuel (+ acc fuel))
        acc))))

(s/fdef part1
  :args (s/cat :weight ::weight)
  :ret ::fuel)

(-> (io/resource "day01.txt")
    slurp
    str/trim
    (str/split #"\s+")
    (->> (map #(Integer/parseInt %)))
    (->> (map part1))
    ;; (->> (map part2))
    (->> (apply +)))
