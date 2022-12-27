(ns ionsails.core.systems.nutrition
  (:require [ionsails.core.systems.quantifiable :as quant]))

(defn can-drink?
  [vessel-details]
  (and
   (:holds-liquid? vessel-details)
   (filter
    :edible?
    (quant/get-liquids vessel-details))))

(defn can-eat?
  [ent-details]
  (:edible? ent-details))
