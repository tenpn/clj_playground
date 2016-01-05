(ns clj-playground.core
  (:require [clojure.set :as set]))

(defn cell 
  "coordinates of a map cell"
  [x y]
  {:x x :y y})

(def diagonal-cost (Math/sqrt 2))

(defn traversal [cost cell-coord]
  {:cell cell-coord :cost cost})

(defn traversal-cost [from-cell to-cell]
  (let [offset (cell (- (:x to-cell) (:x from-cell)) (- (:y to-cell) (:y from-cell)))]
    (Math/sqrt (+ (* (:x offset) (:x offset)) (* (:y offset) (:y offset))))))

(defn translate-cell [translate origin]
  (cell (+ (:x origin) (:x translate))
        (+ (:y origin) (:y translate))))

(defn create-grid  
  "creates grid of set dimensions. width and height are exclusive."
  [width height]
  {:width width :height height})

(defn valid-cell? [grid c]
  {:pre [(not-any? nil? [grid c])]}
  (and (< -1 (:x c) (:width grid))
       (< -1 (:y c) (:height grid))))

(defn neighbours-of 
  "neighbours around square 2D grid cell. returns {:neighbours :traversals} 
  where :neighbours is a set of neighbouring absolute cells
  and :traversals is a map of cells to costs"
  [grid origin]
  {:pre [(valid-cell? grid origin)]}
  (let [valid-neighbours (->> [(cell  -1 -1) (cell 0 -1) (cell 1 -1)
                               (cell -1 0) (cell 1 0)
                               (cell -1 1) (cell 0 1) (cell 1 1)]
                              (map (partial translate-cell origin))
                              (filter (partial valid-cell? grid)))]
    {:neighbours (set valid-neighbours)
     :traversals (reduce #(assoc %1 %2 (traversal-cost %2 origin)) {} valid-neighbours)}))

;; (neighbours-of (create-grid 3 3) (cell 1 1))

(defn create-route 
  "takes a parents map of form {child parent} and finds route from current-cell to first nil parent."
  [parents current-cell]
  {:pre [(not-any? nil? [parents current-cell])]
   :post [(some? %)]}
  (loop [current-cell current-cell
         created-route '()]
    (if-let [parent (parents current-cell)]
      (recur parent
             (conj created-route current-cell))
      ;; else parent is nil, and it's the end of the road
      created-route))) 

(defn navigate-to 
  "provides route from start to dest, not including start"
  [grid start dest]
  {:pre [(valid-cell? grid start)
         (valid-cell? grid dest)]}
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
                                   (map :cell)
                                   set)
            updated-open (map (fn [{open-cell :cell current-cost :cost :as open-node}]
                               (if (contains? better-neighbours open-cell)
                                 (traversal (neighbour-total-distances open-cell) open-cell)
                                 open-node))
                              open-nodes)
            updated-parents (reduce #(assoc %1 %2 current-cell) {} better-neighbours)
            ;; ...then create new neighbours
            all-open-cells (set (map :cell open-nodes))
            new-neighbours (set/difference unvisited-neighbours all-open-cells)
            new-open-nodes (map #(traversal (neighbour-total-distances %) %) new-neighbours)
            new-parents (reduce #(assoc %1 %2 current-cell) {} new-neighbours)]
        (recur (sort-by :cost (concat updated-open new-open-nodes))
               (conj visited-cells current-cell)
               (merge parents updated-parents new-parents))))))

;;(navigate-to (create-grid 5 5) (cell 0 0) (cell 0 4))

