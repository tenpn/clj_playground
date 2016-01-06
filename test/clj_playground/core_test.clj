(ns clj-playground.core-test
  (:require [clojure.test :refer :all]
            [clj-playground.core :refer :all]))

(deftest grid-creation
  (is (= (create-grid 3 3 :obstacles #{(cell 1 1)})
         (create-grid 3 3 :obstacles [(cell 1 1)]))))

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
    
    (is (= {:neighbours #{left-top mid-top right-top
                          left-mid right-mid
                          left-bot mid-bot right-bot}
            :traversals {left-top diagonal-cost, mid-top 1.0, right-top diagonal-cost,
                         left-mid 1.0, right-mid 1.0,
                         left-bot diagonal-cost, mid-bot 1.0, right-bot diagonal-cost}}
           (neighbours-of grid-3x3 mid-mid))
        "all eight neighbours")
    (is (= {:neighbours #{left-top mid-top
                          mid-mid
                          left-bot mid-bot}
            :traversals {left-top 1.0, mid-top diagonal-cost,
                         mid-mid 1.0,
                         left-bot 1.0, mid-bot diagonal-cost}}
           (neighbours-of grid-3x3 left-mid))
        "left edge")
    (is (= {:neighbours #{mid-top right-top
                          mid-mid
                          mid-bot right-bot}
            :traversals {mid-top diagonal-cost, right-top 1.0,
                         mid-mid 1.0,
                         mid-bot diagonal-cost, right-bot 1.0}}
           (neighbours-of grid-3x3 right-mid))
        "right edge")
    (is (= {:neighbours #{left-top right-top
                          left-mid mid-mid right-mid}
            :traversals {left-top 1.0, right-top 1.0,
                         left-mid diagonal-cost, mid-mid 1.0, right-mid diagonal-cost}}
           (neighbours-of grid-3x3 mid-top))
        "top edge")
    (is (= {:neighbours #{left-mid mid-mid right-mid
                          left-bot right-bot}
            :traversals {left-mid diagonal-cost, mid-mid 1.0, right-mid diagonal-cost,
                         left-bot 1.0, right-bot 1.0}}
           (neighbours-of grid-3x3 mid-bot))
        "bottom edge")))

(deftest navigation
  (let [big-grid (create-grid 5 5)
        some-start (cell 0 0)
        some-neighbour (cell 1 0)]
    (testing "simple navigation"
      (is (= []
             (navigate-to big-grid some-start some-start))
          "when navigating to self, route is empty")
      (is (= [some-neighbour]
             (navigate-to big-grid some-start some-neighbour))
          "when navigating to neighbour, only destination in route")
      (is (= [(cell 1 0) (cell 2 0)]
             (navigate-to big-grid (cell 0 0) (cell 2 0)))
          "straight simple route")
      (is (= [(cell 1 1) (cell 2 2)]
             (navigate-to big-grid (cell 0 0) (cell 2 2)))
          "another simple route")
      (is (= [(cell 0 1) (cell 0 2)]
             (navigate-to big-grid (cell 0 0) (cell 0 2)))
          "shortest route")))
  (let [blocked-grid (create-grid 5 5 :obstacles [(cell 1 0)])]
    (testing "obstacles"
      (is (= [(cell 1 1) (cell 2 0)]
             (navigate-to blocked-grid (cell 0 0) (cell 2 0)))
          "obstacles are routed around"))))
             
