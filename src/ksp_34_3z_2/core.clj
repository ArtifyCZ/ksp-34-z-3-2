(ns ksp-34-3z-2.core
  (:require [clojure.string :as str]))

(defn read-all-input-lines [num]
  (-> (str "resources/" num ".in")
      slurp
      str/split-lines))

(defn parse-first-row [lines]
  (-> lines
      first
      (str/split #"\ ")
      (#(map read-string %))))

(defn parse-a-display [display]
  (->> display
       (map-indexed (fn [y a]
                      (map-indexed (fn [x b]
                                     (when-not (= b \.)
                                       [x y])) a)))
       (apply concat)
       (remove nil?)
       set))

(defn parse-display-rows [lines r n]
  (let [[samples to-solve] (->> (rest lines)
                                (partition-all r)
                                (map parse-a-display)
                                (split-at n))]
    [samples to-solve]))

(defn clear? [samples to-solve]
  (->> samples
       (filter (fn [s]
                 (every? true? (map #(contains? s %) to-solve))))
       count
       (> 2)))

(defn -main [num]
  (let [lines (read-all-input-lines num)
        [r _ n _] (parse-first-row lines)
        [samples to-solve] (parse-display-rows lines r n)]
    (->> to-solve
         (map #(if (clear? samples %) "ANO" "NE"))
         (str/join "\n")
         (#(str % "\n"))
         (spit (str "resources/" num ".out")))))
