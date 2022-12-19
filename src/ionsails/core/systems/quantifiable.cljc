(ns ionsails.core.systems.quantifiable
  (:require [ionsails.util.noise.rand :as r]))

;; TODO: move to a util
(defn floor [v]
  #?(:clj (Math/floor v)
     :cljs (.floor js/Math v)))

(defn find-consume-all-map
  "Given a set of entities, derive the consumption map that correlates to consuming all of them"
  [q-ents]
  (into {} (map (fn [{:keys [template-id quantity]}]
                  [template-id quantity])
                q-ents)))

(defn find-consumed-amounts
  "Given a target amount to consume and a list of quantifiable things,
  return a mapping of how much should be consumed from each, taking an
  equal proportion, but not necessarily an equal quantity of each."
  [desired-consumption q-ents]
  (if (or (zero? desired-consumption) (empty? q-ents))
    {}
    (let [amounts (->> q-ents
                       (map :quantity)
                       (filter identity))
          n-ents (count amounts)
          total-amounts (apply + amounts)]
      ;; Safety measure in case 0 quantity entity isn't deleted.
      (if (< total-amounts 1)
        {}
        ;; Bifurcate the strategy based on the desired amount vs. the number of unique entities to pull from.

        (if (< desired-consumption n-ents)
            ;; If more entities than desired consumption, pick one at random weighted by their respective quantities
          (let [weights-map (into {} (map (juxt :template-id :quantity) q-ents))
                chosen-template-id (r/weighted-choice weights-map)]
            {chosen-template-id (min desired-consumption (get weights-map chosen-template-id))})
            ;; Otherwise pull some from each weighted by respective quantities
          (let [percentages (map #(/ % total-amounts) amounts)
                consumption (min desired-consumption total-amounts)
                per-liquid-map (into {} (map (fn [ent v]
                                               [(:template-id ent) (floor (* v consumption))])
                                             q-ents percentages))
                total (apply + (vals per-liquid-map))]
            (if (and (zero? total) (not-empty per-liquid-map))
                ;; If all liquids are floored to zero, pick one at random to pull 1 from
              (assoc per-liquid-map ((ffirst per-liquid-map)) 1)
              per-liquid-map)))))))

(defn consume-entity-actions
  "Derive actions needed to consume from `consumable-ents` given the mapping
  provided by `consumed-amounts-map` whose keys are template-id's and values
  are quantities to be deducted."
  [consumable-ents consumed-amounts-map]
  (mapv (fn [ent]
          (let [ent-id (:db/id ent)
                {:keys [template-id quantity]} ent
                consumed-amount (get consumed-amounts-map template-id 0)
                new-quantity (- quantity consumed-amount)]
            (if (> quantity consumed-amount)
              {:name :upsert
               :args
               {:entity ent-id
                :attribute :quantity
                :value new-quantity}}
              {:name :delete
               :args {:entity ent-id}})))
        consumable-ents))

(defn refill-entity-actions
  [target-ent-id source-ents target-ents additional-amounts-map]
  (mapv (fn [src-ent]
          (let [template-id (:template-id src-ent)
                refill-amount (get additional-amounts-map template-id 0)
                existing-match (first (filter (fn [target-ent]
                                                (= template-id (:template-id target-ent)))
                                              target-ents))]
            (if-not existing-match
              (let [new-id -1
                    new-ent (assoc src-ent :db/id new-id
                                   :quantity refill-amount)]
                {:name :create
                 :args new-ent
                 :after-tx
                 (fn [tempids-map]
                   (when-let [value (get tempids-map new-id)]
                     [{:name :upsert
                       :args [{:entity target-ent-id
                               :attribute :contents
                               :value value}]}]))})
              (let [quantity (:quantity existing-match)
                    new-quantity (+ quantity refill-amount)]
                {:name :upsert
                 :args
                 {:entity (:db/id existing-match)
                  :attribute :quantity
                  :value new-quantity}}))))
        source-ents))

(defn get-liquids
  [vessel-details]
  (->> vessel-details
       :contents
       (filter :liquid?)))
