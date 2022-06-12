(ns ionsails.core.timers
  (:require [ionsails.core.queries :as q]
            [ionsails.core.systems.flammable :as flam]
            [ionsails.core.systems.timers :as stimers]
            [ionsails.core.effects :as e]))

(defmulti tick-actions
  (fn [db ent]
    (:tick-action ent)))


(defmethod tick-actions :default
  [db {:keys [:db/id tick-action parent]}]
  (prn "ERROR: No tick-action defined for " tick-action " for timer " id " on " parent ""))

(defmethod tick-actions :consume-fuel-contents
  [db {:keys [:db/id tick-recur tick-next parent]}]
  (let [container (q/vessel-details db parent)
        ;; TODO: Add verification the container is still lit
        flam-ents (flam/get-flammables container)
        {:keys [consumed-amounts burn-rate]} (flam/find-burned-amounts flam-ents)
        effects (e/consume-by-template-mapping flam-ents consumed-amounts)
        effects (conj effects (stimers/recur-timer id (+ tick-next burn-rate)))
        effects (cond-> effects
                        (not= burn-rate tick-recur) (conj (stimers/set-recur id burn-rate)))]
    effects))
