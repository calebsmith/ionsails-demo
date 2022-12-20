(ns ionsails.core.systems.flammable
  (:require [ionsails.core.systems.quantifiable :as quant]
            [ionsails.core.effects :as e]))

(defn flammable?
  [ent]
  (some? (:burn-rate ent)))

(defn get-flammables
  [vessel-details]
  (->> vessel-details
       :contents
       (filter flammable?)))

(defn get-non-flammables
  [vessel-details]
  (->> vessel-details
       :contents
       (remove flammable?)))

(defn can-light?
  [item]
  (or (flammable? item)
      (and
        (not-empty (get-flammables item))
        (empty? (get-non-flammables item)))))

(defn find-burned-amounts
  "Given a set of flammable entities, choose one at random, consume 1 of it, and find its burn-rate (number of ticks before consuming more)"
  [flam-ents]
  (let [consumed-amounts-map (quant/find-consumed-amounts 1 flam-ents)
        chosen-id (first (keys consumed-amounts-map))
        chosen-ent (first (get (group-by :template-id flam-ents) chosen-id))]
    {:burn-rate (:burn-rate chosen-ent)
     :consumed-amounts consumed-amounts-map}))

(defn- set-burn-state
  [ent-id state]
  (if state
    (e/set-state ent-id :burning? state)
    (e/remove-attr ent-id :burning?)))

(defn- create-burn-timer
  [db ent-id burn-rate]
  (e/create-timer db ent-id :consume-fuel-contents burn-rate true))

(defn- remove-burn-timer
  [ent]
  (let [burn-timer-ids (->> (:timers ent)
                            (filter #(= (:tick-action %) :consume-fuel-contents))
                            (map :db/id))]
    (map e/purge burn-timer-ids)))

(defn set-extinguish
  [ent]
  (conj
   (remove-burn-timer ent)
   (set-burn-state (:db/id ent) false)))

(defn set-burn
  [db ent-id burn-rate]
  [(set-burn-state ent-id true)
   (create-burn-timer db ent-id burn-rate)])

(defn burning?
  [ent]
  (:burning? ent))

(defn resolve-consume-source
  [ent consumption-map]
  (let [liquids (quant/get-liquids ent)
        src-remaining (reduce
                       (fn [acc {:keys [template-id quantity]}]
                         (let [amount (get consumption-map template-id 0)
                               remaining (- quantity amount)]
                           (+ acc remaining)))
                       0
                       liquids)]
    (when (< src-remaining 1)
      (set-extinguish ent))))

(defn resolve-consume-target
  [ent src-liquids]
  (let [non-flam-liquids (remove flammable? src-liquids)]
    (when-not (empty? non-flam-liquids)
      (set-extinguish ent))))
