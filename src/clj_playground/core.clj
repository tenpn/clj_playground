(ns clj-playground.core
  (:require [clojure.set :as set]))

(defn cell [x y]
  "coordinates of a map cell"
  {:x x :y y})

(def diagonal-cost (Math/sqrt 2))

(defn traversal [cost cell-coord]
  {:cell cell-coord :cost cost})

(defn traversal-cost [from-cell to-cell]
  (let [offset (cell (- (to-cell :x) (from-cell :x)) (- (to-cell :y) (from-cell :y)))]
    (Math/sqrt (+ (* (offset :x) (offset :x)) (* (offset :y) (offset :y))))))

(defn translate-cell [origin translate]
  (cell (+ (origin :x) (translate :x))
        (+ (origin :y) (translate :y))))

(defn create-grid [width height]
  "creates grid of set dimensions. width and height are exclusive."
  {:width width :height height})

(defn valid-cell? [grid c]
  (and (>= (c :x) 0)
       (< (c :x) (grid :width))
       (>= (c :y) 0)
       (< (c :y) (grid :height))))

(definline validate-cell [grid c]
  "asserts if any part of the cell isn't in the grid"
  (assert (>= (c :x) 0))
  (assert (< (c :x) (grid :width)))
  (assert (>= (c :y) 0))
  (assert (< (c :y) (grid :height))))

(defn neighbours-of [grid origin]
  "neighbours around square 2D grid cell. returns {:neighbours :traversals} 
where :neighbours is a set of neighbouring absolute cells
and :traversals is a map of cells to costs ??"
  (validate-cell grid origin)
  (let [valid-neighbours (->> [(cell  -1 -1) (cell 0 -1) (cell 1 -1)
                               (cell -1 0) (cell 1 0)
                               (cell -1 1) (cell 0 1) (cell 1 1)]
                              (map #(translate-cell % origin))
                              (filter #(valid-cell? grid %)))]
    {:neighbours (set valid-neighbours)
     :traversals (reduce #(assoc %1 %2 (traversal-cost %2 origin)) {} valid-neighbours)}))

;; (neighbours-of (create-grid 3 3) (cell 1 1))

(defn create-route [parents current-cell]
  "takes a parents list of form {child parent} and finds route from current-cell to first nil parent"
  (loop [current-cell current-cell
         created-route '()]
    (let [parent (parents current-cell)]
      (if (nil? parent) ;; at end of the road
        created-route 
        (recur parent
               (conj created-route current-cell))))))

(defn navigate-to [grid start dest]
  "provides route from start to dest, not including start"
  (validate-cell grid start)
  (validate-cell grid dest)
  (loop [[{current-cell :cell current-cost :cost} & open-nodes] (list (traversal 0 start))
         visited-cells #{}
         parents {}]
    (if (= dest current-cell) 
      (create-route parents current-cell) 
      (let [;; traversals is list of {:cell :cost}
            {all-neighbours :neighbours traversals :traversals} (neighbours-of
                                                                 grid
                                                                 current-cell)
            unvisited-neighbours (set/difference all-neighbours visited-cells)
            ;; add current route cost to traversals:
            neighbour-total-distances (into {}
                                            (for [[neighbour cost] traversals]
                                              [neighbour (+ current-cost cost)]))
            ;; keep bigger distances
            ;; ...first merge in seen neighbours, keeping higher values
            better-neighbours (->> open-nodes
                                   (filter
                                    (fn [{open-cell :cell current-cost :cost :as open-node}]
                                      (and (contains? unvisited-neighbours open-cell)
                                           (> current-cost
                                              (neighbour-total-distances open-cell)))))
                                   (map #(% :cell))
                                   set)
            updated-open (map (fn [{open-cell :cell current-cost :cost :as open-node}]
                               (if (contains? better-neighbours open-cell)
                                 (traversal (neighbour-total-distances open-cell) open-cell)
                                 open-node))
                              open-nodes)
            updated-parents (reduce #(assoc %1 %2 current-cell) {} better-neighbours)
            ;; ...then create new neighbours
            all-open-cells (set (map #(% :cell) open-nodes))
            new-neighbours (set/difference unvisited-neighbours all-open-cells)
            new-open-nodes (map #(traversal (neighbour-total-distances %) %) new-neighbours)
            new-parents (reduce #(assoc %1 %2 current-cell) {} new-neighbours)]
        (recur (sort-by :cost (concat updated-open new-open-nodes))
               (conj visited-cells current-cell)
               (merge parents updated-parents new-parents))))))

;;(navigate-to (create-grid 5 5) (cell 0 0) (cell 0 2))
