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

(defn create-route [parents current-node create-route]
  "takes a parents list of form {child parent} and finds route from current-node to first nil parent"
  (let [parent (parents current-node)]
    (if (nil? parent) ;; at end of the road
      create-route 
      (recur parents
             parent
             (conj create-route current-node)))))

(defn navigate-to [grid start dest]
  "provides route from start to dest, not including start"
  (validate-cell grid start)
  (validate-cell grid dest)
  (loop [[{current-node :cell current-cost :cost} & open-nodes] (list (traversal 0 start))
         visited-nodes #{}
         parents {}]
    (if (= dest current-node) 
      (create-route parents current-node '()) 
      (let [;; traversals is list of {:cell :cost}
            {all-neighbours :neighbours traversals :traversals} (neighbours-of
                                                                 grid
                                                                 current-node)
            unvisited-neighbours (set/difference all-neighbours visited-nodes)
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
            updated-parents (reduce #(assoc %1 %2 current-node) {} better-neighbours)
            ;; ...then create new neighbours
            all-open-nodes (set (map #(% :cell) open-nodes))
            new-neighbours (set/difference unvisited-neighbours all-open-nodes)
            new-open-nodes (map #(traversal (neighbour-total-distances %) %) new-neighbours)
            new-parents (reduce #(assoc %1 %2 current-node) {} new-neighbours)]
        (recur (sort-by :cost (concat updated-open new-open-nodes))
               (conj visited-nodes current-node)
               (merge parents updated-parents new-parents))))))

;;(navigate-to (create-grid 5 5) (cell 0 0) (cell 0 2))
