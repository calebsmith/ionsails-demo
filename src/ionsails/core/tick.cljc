(ns ionsails.core.tick
  (:require [ionsails.core.data :refer [ get-conn]]
            [ionsails.core.queries :as q]
            [ionsails.core.actions :as a]
            [ionsails.core.systems.timers :as timers]
            [ionsails.core.timers :refer [tick-actions]]))

(defn run-timers!
  [db timers-map]
  (apply concat
         (for [[tick-action timer-ents] timers-map]
           (mapv
            (fn [timer-ent]
              (tick-actions db timer-ent))
            timer-ents))))

(defn run-tick!
  []
  (let [conn (get-conn)
        db @conn
        {:keys [id value]} (q/get-tick db)
        timers-map (q/get-timers db value)
        timer-effects (apply concat (run-timers! db timers-map))
        next-tick-effect (timers/set-next-tick id value)
        effects (conj timer-effects next-tick-effect)]
    (a/apply-effects! conn db nil effects)))
