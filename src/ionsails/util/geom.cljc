(ns ionsails.util.geom)

(defn distance
  [[x y z] [x2 y2 z2]]
  (let [dx (Math/abs (- x x2))
        dy (Math/abs (- y y2))
        dz (Math/abs ((fnil - 0 0) z z2))]
    (Math/sqrt
     (+  (* dx dx)
         (* dy dy)
         (* dz dz)))))
