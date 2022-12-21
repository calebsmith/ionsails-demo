(ns ionsails.core.systems.timers
  (:require [ionsails.core.transactions :as t]))

(defn inc-tick
  [value]
  (t/upsert [:counter :global] :tick (inc value)))

(defn recur-timer
  [ent-id value]
  (t/upsert ent-id :tick-next value))

(defn set-recur
  [ent-id value]
  (t/upsert ent-id :tick-recur (or value 1)))
