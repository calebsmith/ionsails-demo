(ns ionsails.core.timers
  (:require [ionsails.core.queries :as q]
            [ionsails.core.messages :as m]
            [ionsails.core.titles :as ti]
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
        container-title (ti/get-title container)]
    (if-not (flam/burning? container)
      ;; Safety measure just in case timer activated on extinguished entity
      (e/purge id)
      (let [flam-ents (flam/get-flammables container)
            {:keys [consumed-amounts burn-rate]} (flam/find-burned-amounts flam-ents)]
        (if (or (empty? flam-ents) (nil? burn-rate))
          (assoc
           (m/dm-err "Your " (ti/get-title-no-article container) " goes out")
           :effects (flam/set-extinguish container))
          (let [remaining-fuel (apply + (map :quantity flam-ents))
                effects (e/consume-by-template-mapping flam-ents consumed-amounts)
                effects (conj effects (stimers/recur-timer id (+ tick-next burn-rate)))
                effects (cond-> effects
                          (not= burn-rate tick-recur) (conj (stimers/set-recur id burn-rate)))
                res {:effects effects}
                res (cond-> res
                            (< remaining-fuel 5) (merge (m/dm-warn "The flame on your " container-title " flickers for a moment.")))]
            res))))))
