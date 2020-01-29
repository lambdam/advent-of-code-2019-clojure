(ns aoc.day02
  (:require [clojure.alpha.spec :as s]
            [clojure.alpha.spec.test :as st]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(s/def ::instructions
  (s/coll-of integer?))

(defn instructions-str->vec [string]
  (-> string
      str/trim
      (str/split #",")
      (->> (mapv #(Integer/parseInt %)))))

(s/fdef instructions-str->vec
  :args string?
  :ret ::instructions)

(defn eval-instructions [{:keys [noun verb instructions]}]
  (-> instructions
      (assoc 1 noun)
      (assoc 2 verb)
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
      (get 0)))

(s/fdef eval-instructions
  :args (s/cat :data (s/schema {:noun integer?
                                :verb integer?
                                :instructions ::instructions}))
  :ret integer?)

(st/instrument `eval-instructions)

(comment ;; part 1
  (eval-instructions
    {:noun 12
     :verb 2
     :instructions (-> "day02.txt" io/resource slurp instructions-str->vec)})
  (take 3 (-> "day02.txt" io/resource slurp instructions-str->vec))
  )

(defn look-for-result [{:keys [instructions target]}]
  (loop [noun 0
         verb 0]
    (cond
      (> noun 99) nil
      (> verb 99) (recur (inc noun) 0)
      :else (let [result (eval-instructions {:noun noun
                                             :verb verb
                                             :instructions instructions})]
              (if (= result target)
                {:noun noun
                 :verb verb}
                (recur noun (inc verb)))))))

(s/fdef look-for-result
  :args (s/cat :data (s/schema {:instructions ::instructions
                                :target integer?}))
  :ret (s/schema {:noun integer?
                  :verb integer?}))

(st/instrument `look-for-result)

(comment ;; part 2
  (look-for-result
    {:target 19690720
     :instructions (-> "day02.txt" io/resource slurp instructions-str->vec)})
  )
