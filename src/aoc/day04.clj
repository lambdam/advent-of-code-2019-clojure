(ns aoc.day04)

(def start 123257)

(def stop 647015)

(defn sequential-digits? [number]
  (let [s (str number)]
    (not= (count s) (-> s dedupe count))))

(defn no-decreasing-digit? [number]
  (->> number
       str
       seq
       (transduce (comp (map str)
                        (map #(Integer/parseInt %)))
                  (fn test
                    ([data] data) ;; final step
                    ([{:keys [last status] :as acc} x]
                     (if (< x last)
                       (reduced (assoc acc :status false))
                       (assoc acc :last x))))
                  {:last 0
                   :status true})
       :status))

(comment
  ;; part 1
  (loop [x start
         acc []]
    (cond
      (> x stop) (count acc)
      ;; ---
      (and (sequential-digits? x)
           (no-decreasing-digit? x))
      (recur (inc x) (conj acc x))
      ;; ---
      :else (recur (inc x) acc)))
  )
