(ns ionsails.util.noise.rand)

(defn weighted-choice
  "Given a map whose values"
  [weights-map]
  (let [weights (reductions + (vals weights-map))
        total (last weights)
        choice-pairs (map vector (keys weights-map) weights)
        choice (rand-int total)]
    (loop [[[k v] & more] choice-pairs]
      (if (< choice v)
        k
        (recur more)))))
