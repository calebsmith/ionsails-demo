(ns ionsails.core.effects
  (:require [ionsails.core.transactions :as t]
            [ionsails.core.queries :as q]
            ))

(declare place-entity-in)
(declare consume)

(defn- create-within-no-merge
  [new-ent containing-ent-id]
  (let [new-id (t/new-id)
        new-ent (assoc new-ent :db/id new-id)]
    (t/create new-ent
              (fn [_ created-ent-id]
                (t/upsert containing-ent-id :contents created-ent-id)))))

(defn- find-merge-ent
  [ent target-ent]
  (first (filter (fn [{:keys [quantity template-id] :as t-ent}]
                   (and
                    (not= (:db/id ent) (:db/id t-ent))
                    (some? quantity)
                    (= (:template-id ent) template-id)))
                 (:contents target-ent))))

(defn- remove-entity-from-contents
  [ent loc-id]
  (t/retract loc-id :contents (:db/id ent)))

(defn- place-entity-in
  [ent target-ent]
  (if (nil? (:quantity ent))
    (t/upsert (:db/id target-ent) :contents (:db/id ent))
    (let [existing-match (find-merge-ent ent target-ent)]
      (if-not existing-match
        (create-within-no-merge ent (:db/id target-ent))
        (let [quantity (:quantity existing-match)
              new-quantity (+ quantity (:quantity ent))]
          (t/upsert (:db/id existing-match) :quantity new-quantity))))))

(defn create-within
  [ent containing-ent]
  (if (some? (:quantity ent))
    (place-entity-in ent containing-ent)
    (create-within-no-merge ent containing-ent)))

(defn move-contents
  [ent src target]
  (let [removal-op (if (some? (:quantity ent))
                     (consume src (:quantity ent))
                     (remove-entity-from-contents ent src))]
    [removal-op
     (place-entity-in ent target)]))

(defn move-contents-no-merge
  [ent-id src-id target-id]
  [(t/retract src-id :contents ent-id)
   (t/upsert target-id :contents ent-id)])

(defn equip
  [ent-id src-id target-id]
  [(t/retract src-id :contents ent-id)
   (t/upsert target-id :equips ent-id)])

(defn unequip
  [ent-id src-id target-id]
  [(t/retract src-id :equips ent-id)
   (t/upsert target-id :contents ent-id)])

(defn consume
  [ent amount]
  (let [ent-id (:db/id ent)
        quantity (:quantity ent)]
    (if (> quantity amount)
      (t/upsert ent-id :quantity (- quantity amount))
      (t/delete ent-id))))

(defn consume-by-template-mapping
  [ents consumption-map]
  (mapv (fn [ent]
          (let [amount (get consumption-map (:template-id ent) 0)]
            (consume ent amount)))
        ents))

(defn create-by-template-mapping
  [ents container-ent consumption-map]
  (let [new-sl (map (fn [{:keys [template-id] :as l}]
                      (let [amount (get consumption-map template-id)]
                        (assoc l :quantity amount))) ents)]
    (map #(create-within % container-ent) new-sl)))

(defn create-timer
  [containing-ent-id action duration & [recur?]]
  (fn [db]
    (let [timer-id (t/new-id)
          tick (q/get-tick db)
          timer {:db/id timer-id
                 :tick-action action
                 :tick-next (+ tick duration)}
          timer (cond-> timer
                        recur? (assoc :tick-recur duration))]
      (t/create timer
                (fn [_ created-ent-id]
                  (t/upsert containing-ent-id :timers created-ent-id))))))


;; Nothing interesting for now. Are these necessary?
(defn set-state
  [ent-id attr v]
  (t/upsert ent-id attr v))

(defn purge
  [ent-id]
  (t/delete ent-id))

(defn remove-attr
  [ent-id attr]
  (t/delete-attr ent-id attr))
