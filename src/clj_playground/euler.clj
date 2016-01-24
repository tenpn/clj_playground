(ns clj-playground.euler
  (:require [clojure.set :as set]))

;; problem 1

(defn p1-is-interesting-multiple [num]
  (or 
   (zero? (rem num 3))
   (zero? (rem num 5))))

(defn p1-sum-interesting-multiples-below [threshold]
  (->> (range threshold)
       (filter p1-is-interesting-multiple)
       (reduce +)))

(comment (=
          233168
          (p1-sum-interesting-multiples-below 1000)))

;; problem 2

(defn p2-fibonacci
  "makes a lazy sequence"
  ([] (p2-fibonacci 1 2))
  ([a b] (lazy-seq (cons a (p2-fibonacci b (+ a b))))))

(defn p2-sum-of-even-fibs-below [limit]
  (->> (p2-fibonacci)
       (take-while (partial > limit))
       (filter even?)
       (reduce +)))

(comment
  (= 4613732
     (p2-sum-of-even-fibs-below 4000000)))
