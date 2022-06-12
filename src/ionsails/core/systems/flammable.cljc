(ns ionsails.core.systems.flammable
  (:require [ionsails.core.systems.quantifiable :as quant]))

(defn get-flammables
  [vessel-details]
  (->> vessel-details
       :contents
       (filter :burn-rate)))

(defn find-burned-amounts
  "Given a set of flammable entities determine the combined burn rate (number of ticks before consuming more)
  and consume 1 of each."
  [flam-ents]
  (let [burn-rate (apply + (map :burn-rate flam-ents))
        consumed-amounts-map (quant/find-consumed-amounts (count flam-ents) flam-ents)]
    {:burn-rate burn-rate
     :consumed-amounts consumed-amounts-map}))
