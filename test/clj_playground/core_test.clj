(ns clj-playground.core-test
  (:require [clojure.test :refer :all]
            [clj-playground.core :refer :all]))

(deftest neighbours-of-t
  (let [grid-3x3 (create-grid 3 3)

        left-top (cell 0 0)
        mid-top (cell 1 0)
        right-top (cell 2 0)

        left-mid (cell 0 1)
        mid-mid (cell 1 1)
        right-mid (cell 2 1)

        left-bot (cell 0 2)
        mid-bot (cell 1 2)
        right-bot (cell 2 2)]
    
    (is (= #{(traversal-diag left-top) (traversal-unit mid-top) (traversal-diag right-top)
             (traversal-unit left-mid) (traversal-unit right-mid)
             (traversal-diag left-bot) (traversal-unit mid-bot) (traversal-diag right-bot)}
           (neighbours-of grid-3x3 mid-mid))
        "all eight neighbours")
    (is (= #{(traversal-unit left-top) (traversal-diag mid-top)
             (traversal-unit mid-mid)
             (traversal-unit left-bot) (traversal-diag mid-bot)}
           (neighbours-of grid-3x3 left-mid))
        "left edge")
    (is (= #{(traversal-diag mid-top) (traversal-unit right-top)
             (traversal-unit mid-mid)
             (traversal-diag mid-bot) (traversal-unit right-bot)}
           (neighbours-of grid-3x3 right-mid))
        "right edge")
    (is (= #{(traversal-unit left-top) (traversal-unit right-top)
             (traversal-diag left-mid) (traversal-unit mid-mid) (traversal-diag right-mid)}
           (neighbours-of grid-3x3 mid-top))
        "top edge")
    (is (= #{(traversal-diag left-mid) (traversal-unit mid-mid) (traversal-diag right-mid)
             (traversal-unit left-bot) (traversal-unit right-bot)}
           (neighbours-of grid-3x3 mid-bot))
        "bottom edge")))
