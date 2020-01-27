(ns aoc.day03
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]))

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

(defn instructions->positions
  "An instruction has the form [direction as char, steps as int]"
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

(def first-wire
  "The format is a map of :x :y final positions and a :positions map of {[x y] number-of-passes}"
  (-> data
      first
      instructions->positions))

(def second-wire
  "The format is a map of :x :y final positions and a :positions map of {[x y] number-of-passes}"
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
