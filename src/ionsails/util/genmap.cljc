(ns ionsails.util.genmap
  (:require [ionsails.util.noise.gen :as gen_noise]))


(def tileset [
  [" " :black :black]
  [" " :black :blue]
  ["." :yellow :green]
  ["." :black :green]
  ["#" :yellow :green]
  ["@" :yellow :green]
  ["^" :yellow :green]
])

(def board-width 80)
(def board-height 40)

(defn get-tile-values [width height]
  (map (fn [[x y val]]
         (int (Math/round (* 5 (/ (inc val) 2)))))
       (gen_noise/octave-perlin-over-ranges 6 0.125 (/ 1 16) width height)))

(defn default-game-board []
  (partition-all board-width (get-tile-values board-width board-height)))

(comment

  (doseq [row (map
               (fn [row]
                 (apply str
                        (map
                         (fn [n]
                           (let [tile (nth tileset n)]
                             (first tile)))
                         row)))
               (default-game-board))]
    (println row))
  )
