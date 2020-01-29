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

(s/def ::positions
  (s/map-of (s/cat :x integer?
                   :y integer?)
            integer?))

(defn process-line [line]
  (let [instructions (str/split line #",")]
    (->> instructions
         (mapv (fn [instruction]
                 [(nth instruction 0)
                  (Integer/parseInt (subs instruction 1))])))))

(def main-data
  (->> (io/resource "day03.txt")
       slurp
       str/trim
       str/split-lines
       (mapv process-line)))

(def test-data-1
  (->> "R75,D30,R83,U83,L12,D49,R71,U7,L72
        U62,R66,U55,R34,D71,R55,D58,R83"
       str/split-lines
       (map str/trim)
       (map process-line)))

(def test-data-2
  (->> "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51
        U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"
       str/split-lines
       (map str/trim)
       (map process-line)))

(comment
  (s/valid? ::data main-data)
  )

(defn instructions->positions
  [instructions]
  (loop [x 0
         y 0
         positions {}
         instructions (apply list instructions)]
    (if (empty? instructions)
      positions
      (let [[direction steps] (peek instructions)
            tail (pop instructions)]
        (cond
          (zero? steps) (recur x y positions tail)
          :else (let [new-instructions (conj tail [direction (dec steps)])
                      new-positions (update positions [x y] #(if (nil? %) 1 (inc %)))]
                  (case direction
                    \L (recur (dec x) y new-positions new-instructions)
                    \U (recur x (inc y) new-positions new-instructions)
                    \R (recur (inc x) y new-positions new-instructions)
                    \D (recur x (dec y) new-positions new-instructions))))))))

(s/fdef instructions->positions
  :args (s/cat :instructions ::instructions)
  :ret ::positions)

(st/instrument `instructions->positions)

(defn abs [x]
  (max x (- x)))

(s/fdef abs
  :args (s/cat :x integer?)
  :ret integer?)

(defn intersections [data]
  (-> [(->> data
            first
            instructions->positions
            (map first)
            set)
       (->> data
            second
            instructions->positions
            (map first)
            set)]
      (->> (apply set/intersection))
      (disj [0 0])))

(defn min-intersection [data]
  (->> data
       intersections
       (map (fn [[x y]]
              (+ (abs x) (abs y))))
       (apply min)))

(comment
  (min-intersection test-data-1) ;; test ok
  (min-intersection test-data-2) ;; test ok
  ;; part 1
  (time (min-intersection main-data))
  )


(defn wire-own-intersections [positions]
  (->> positions
       ;; Transducer here
       (into []
             (comp (filter (fn [[position value]]
                             (> value 1)))
                   (map (fn [[position _]]
                          position))
                   (map (fn [[x y]]
                          (+ (abs x) (abs y))))))))

(comment
  (->> main-data first instructions->positions wire-own-intersections (apply min))
  )

(defn count-steps [{:keys [target instructions]}]
  (loop [x 0
         y 0
         steps-acc 0
         instructions (apply list instructions)]
    (if (empty? instructions)
      nil
      (let [[direction steps] (peek instructions)
            tail (pop instructions)]
        (cond
          (zero? steps) (recur x y steps-acc tail)
          (= [x y] target) steps-acc
          :else (let [new-instructions (conj tail [direction (dec steps)])
                      new-steps (inc steps-acc)]
                  (case direction
                    \L (recur (dec x) y new-steps new-instructions)
                    \U (recur x (inc y) new-steps new-instructions)
                    \R (recur (inc x) y new-steps new-instructions)
                    \D (recur x (dec y) new-steps new-instructions))))))))

(s/fdef count-steps
  :args (s/cat :data (s/schema {:target (s/cat :x integer? :y integer?)
                                :instructions ::instructions}))
  :ret (s/nilable integer?))

(defn shortest-crossing-path [data]
  (->> data
       intersections
       (map (fn [target]
              (+ (count-steps {:target target
                               :instructions (first data)})
                 (count-steps {:target target
                               :instructions (second data)}))))
       (apply min)))

(comment
  (shortest-crossing-path test-data-1) ;; test ok
  (shortest-crossing-path test-data-2) ;; test ok
  ;; part 2
  (shortest-crossing-path main-data)
  )
