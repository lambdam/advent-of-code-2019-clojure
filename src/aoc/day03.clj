(ns aoc.day03
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as st]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

(s/def ::instruction
  (s/cat :direction #{\L \U \R \D}
         :steps integer?))

(s/def ::instructions
  (s/coll-of ::instruction))

(s/def ::data
  (s/coll-of ::instructions))

(s/def ::tracked-positions
  (s/schema {:x integer?
             :y integer?
             :positions (s/map-of (s/cat :x integer?
                                         :y integer?)
                                  integer?)}))

(def data
  (-> (io/resource "day03.txt")
      slurp
      str/trim
      str/split-lines
      (->> (mapv (fn [line]
                   (let [instructions (str/split line #",")]
                     (->> instructions
                          (mapv (fn [instruction]
                                  [(nth instruction 0)
                                   (Integer/parseInt (subs instruction 1))])))))))))

(comment
  (s/valid? ::data data)
  )

(defn instructions->positions
  [instructions]
  (reduce (fn [{:keys [x y positions] :as acc} [direction steps]]
            (case direction
              \L (let [end (- x steps)]
                   {:x end
                    :y y
                    :positions (reduce (fn [acc x']
                                         (update acc [x' y] #(if (nil? %) 1 (inc %))))
                                       positions
                                       (range x end -1))})
              \U (let [end (+ y steps)]
                   {:x x
                    :y end
                    :positions (reduce (fn [acc y']
                                         (update acc [x y'] #(if (nil? %) 1 (inc %))))
                                       positions
                                       (range y end))})
              \R (let [end (+ x steps)]
                   {:x end
                    :y y
                    :positions (reduce (fn [acc x']
                                         (update acc [x' y] #(if (nil? %) 1 (inc %))))
                                       positions
                                       (range x end))})
              \D (let [end (- y steps)]
                   {:x x
                    :y end
                    :positions (reduce (fn [acc y']
                                         (update acc [x y'] #(if (nil? %) 1 (inc %))))
                                       positions
                                       (range y end -1))})
              ;; else
              (throw (Exception. "Not a valid direction"))))
          {:x 0
           :y 0
           :positions {}}
          instructions))

(s/fdef instructions->positions
  :args (s/cat :instructions ::instructions)
  :ret ::tracked-positions)

(st/instrument `instructions->positions)

(def first-wire
  (-> data
      first
      instructions->positions))

(comment
  (s/valid? ::tracked-positions first-wire)
  )

(def second-wire
  (-> data
      second
      instructions->positions))

(def part1
  (let [abs (fn abs [x]
              (max x (- x)))
        first-wire-positions (-> (map first (:positions first-wire))
                                 set)
        second-wire-positions (-> (map first (:positions second-wire))
                                  set)]
    (as-> (set/intersection
            first-wire-positions
            second-wire-positions) <>
      (disj <> [0 0])
      (map (fn [[x y]]
             (+ (abs x) (abs y)))
           <>)
      (apply min <>))))

(comment
  ;; Using transducers to determine the shortest Manhattan path of the crossing
  ;; sections of a unique wire
  (->> (:positions first-wire)
       (into []
             (comp (filter (fn [[position value]]
                             (> value 1)))
                   (map (fn [[position _]]
                          position))
                   (map (fn [[x y]]
                          (+ (abs x) (abs y))))))
       (apply min))
  )
