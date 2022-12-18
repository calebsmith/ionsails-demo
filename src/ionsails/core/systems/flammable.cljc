(ns ionsails.core.systems.flammable
  (:require [ionsails.core.systems.quantifiable :as quant]
            [ionsails.core.effects :as e]))

(defn get-flammables
  [vessel-details]
  (->> vessel-details
       :contents
       (filter :burn-rate)))

(defn can-light?
  [item]
  (some?
   (or
    (:burn-rate item)
    (not-empty
     (get-flammables item)))))

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
  [db ent]
  (let [burn-timer-ids (->> (:timers ent)
                            (filter #(= (:tick-action %) :consume-fuel-contents))
                            (map :db/id))]
    (map e/purge burn-timer-ids)))

(defn set-extinguish
  [db ent]
  (conj
   (remove-burn-timer db ent)
   (set-burn-state (:db/id ent) false)))

(defn set-burn
  [db ent-id burn-rate]
  [(set-burn-state ent-id true)
   (create-burn-timer db ent-id burn-rate)])

(defn burning?
  [ent]
  (:burning? ent))
