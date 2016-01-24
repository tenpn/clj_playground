(ns clj-playground.euler-test
  (:require [clojure.test :refer :all]
            [clj-playground.euler :refer :all]))

(deftest problem-1
  (is (p1-is-interesting-multiple 3))
  (is (false? (p1-is-interesting-multiple 4)))
  (is (p1-is-interesting-multiple 6))
  (is (false? (p1-is-interesting-multiple 1)))
  (is (p1-is-interesting-multiple 5))
  (is (p1-is-interesting-multiple 10))
  (is (= 23
         (p1-sum-interesting-multiples-below 10)))
  (is (= 33
         (p1-sum-interesting-multiples-below 11))))

(deftest problem-2
  (is (= '(1)
         (take 1 (p2-fibonacci))))
  (is (= '(1 2)
         (take 2 (p2-fibonacci))))
  (is (= '(1 2 3 5 8)
         (take 5 (p2-fibonacci))))
  (is (= 0
         (p2-sum-of-even-fibs-below 2)))
  (is (= 2
         (p2-sum-of-even-fibs-below 3)))
  (is (= 10
         (p2-sum-of-even-fibs-below 10))))


