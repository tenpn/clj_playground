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
  [width height & {:keys [obstacles]}]
  {:width width :height height :obstacles (set obstacles)})

(defn valid-cell?
  "false if this cell cannot be moved through, maybe because it's outside the grid"
  [grid c]
  {:pre [(not-any? nil? [grid c])]}
  (and (< -1 (:x c) (:width grid))
       (< -1 (:y c) (:height grid))
       (not (contains? (:obstacles grid) c))))

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

(defn get-better-cells
  "set of cells which have a better route to the goal than current"
  [all-traversals candidate-cells new-costs]
  (->> all-traversals
       (filter
        (fn [{open-cell :cell current-cost :cost}]
          (and (contains? candidate-cells open-cell)
               (> current-cost (new-costs open-cell)))))
       (map :cell)
       set))

(defn merge-costs
  "if traversal is cheaper with candidate, then switch"
  [{open-cell :cell :as original-traversal} better-cells new-costs]
  (if (contains? better-cells open-cell)
    (traversal (new-costs open-cell) open-cell)
    original-traversal))

(defn map-from-list
  "takes a func of form (_ _ -> [k v]) and runs it over a coll to get a map"
  [pair coll]
  (into {} (for [c coll] (pair c))))

(defn navigate-to 
  "provides route from start to dest, not including start"
  [grid start dest]
  {:pre [(valid-cell? grid start)]}
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
            neighbour-new-costs (into {} (for [[neighbour cost] traversals]
                                           [neighbour (+ current-cost cost)]))
            ;; keep bigger distances
            ;; ...first merge in seen neighbours, keeping higher values
            better-neighbours (get-better-cells
                               open-nodes
                               unvisited-neighbours
                               neighbour-new-costs)
            updated-open (map
                          #(merge-costs % better-neighbours neighbour-new-costs)
                          open-nodes)
            updated-parents (map-from-list #(vector % current-cell) better-neighbours)
            ;; ...then create new neighbours
            all-open-cells (set (map :cell open-nodes))
            new-neighbours (set/difference unvisited-neighbours all-open-cells)
            new-open-nodes (map #(traversal (neighbour-new-costs %) %) new-neighbours)
            new-parents (map-from-list #(vector % current-cell) new-neighbours)]
        (recur (sort-by :cost (concat updated-open new-open-nodes))
               (conj visited-cells current-cell)
               (merge parents updated-parents new-parents))))))

;;(navigate-to (create-grid 5 5) (cell 0 0) (cell 0 4))

