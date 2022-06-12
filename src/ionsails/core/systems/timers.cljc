(ns ionsails.core.systems.timers
  (:require
   [ionsails.core.transactions :as t]))

(defn set-next-tick
  [eid value]
  (t/upsert eid :tick (inc value)))

(defn recur-timer
  [ent-id value]
  (t/upsert ent-id :tick-next value))

(defn set-recur
  [ent-id value]
  (t/upsert ent-id :tick-recur (or value 1)))
