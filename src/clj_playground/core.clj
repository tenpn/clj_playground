(ns clj-playground.core
  (:require [clojure.set :as set]))

(defn cell [x y]
  "coordinates of a map cell"
  {:x x :y y})

(def diagonal-cost (Math/sqrt 2))

(defn traversal [cost cell-coord]
  {:cell cell-coord :cost cost})

(defn traversal-unit [cell-coord]
  (traversal 1.0 cell-coord))

(defn traversal-diag [cell-coord]
  (traversal diagonal-cost cell-coord))

(defn traversal-from-offset [offset]
  "takes offset as cell"
  (traversal (Math/sqrt (+ (* (offset :x) (offset :x)) (* (offset :y) (offset :y)))) offset))

(defn translate-traversal [origin translate]
  (traversal (:cost origin)
             (cell (+ ((origin :cell) :x) (translate :x))
                   (+ ((origin :cell) :y) (translate :y)))))

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
  "4 neighbours around square 2D grid cell. set, because we don't care about order."
  (validate-cell grid origin)
  (let [offsets (map traversal-from-offset [(cell  -1 -1) (cell 0 -1) (cell 1 -1)
                                            (cell -1 0) (cell 1 0)
                                            (cell -1 1) (cell 0 1) (cell 1 1)])
        all-neighbours (map #(translate-traversal % origin) offsets)
        valid-neighbours (filter #(valid-cell? grid (% :cell)) all-neighbours)]
    (set valid-neighbours)))

