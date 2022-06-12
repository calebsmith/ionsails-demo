(ns ionsails.util.noise.gen
  (:require [ionsails.util.noise.perlin :as perlin]))

(defn get-range
  ([width]
         (for [x (range width)]
           (vector x)))
  ([width height]
         (for [y (range height)
               x (range width)]
           (vector x y)))
  ([width height depth]
         (for [z (range depth)
               y (range height)
               x (range width)]
           (vector x y z)))
  ([width height depth time4d]
         (for [w (range time4d)
               z (range depth)
               y (range height)
               x (range width)]
           (vector x y z w))))


(defn ranged-noise [noise-func rate & ranges]
  (map (fn [coord]
         (conj coord (apply noise-func
                            (cons
                              (* rate 2 (+ 1 (apply max ranges)))
                              (map inc coord)))))
       (apply get-range ranges)))

(def perlin-over-ranges (partial ranged-noise perlin/perlin))

(defn octave-perlin-over-ranges [octave persistance rate & ranges]
  (apply ranged-noise (concat
                        [
                         (partial perlin/octave-perlin octave persistance)
                         rate]
                        ranges)))

(defn get-noise [x]
  (let [amp 255]
    (Math/round (* amp (perlin/octave-perlin 4 0.25 512 x)))))

(defn get-values [width height]
  (map (fn [[x y val]]
         [x y (int (Math/round (* 256 (/ (inc val) 4))))])
       (octave-perlin-over-ranges 4 0.15 (/ 1 2) width height)))
