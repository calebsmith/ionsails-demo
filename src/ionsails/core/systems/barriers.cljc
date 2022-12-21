(ns ionsails.core.systems.barriers
  (:require
   [clojure.set :as set]
   [ionsails.core.effects :as e]))

(def ^:private long-directions-strs #{"west" "north" "east" "south" "up" "down"})
(def ^:private short-directions-strs #{"w" "n" "e" "s" "u" "d"})
(def directions-strs (set/union long-directions-strs short-directions-strs))
(def directions (set (map keyword long-directions-strs)))

(defn direction-str?
  [str-in]
  (some? (directions-strs str-in)))

(defn direction-keyword?
  [kw]
  (some? (directions kw)))

(defn direction-str->keyword
  [str-in]
  (if (long-directions-strs str-in)
    (keyword str-in)
    (condp = str-in
      "n" :north
      "w" :west
      "s" :south
      "e" :east
      "u" :up
      "d" :down
      nil)))

(defn direction?
  [v]
  (if (string? v)
    (direction-str? v)
    (direction-keyword? v)))

(defn barrier-state-eq
  [state barrier-ent]
  (= (:state barrier-ent) state))

(def open? (partial barrier-state-eq :open))
(def closed? (partial barrier-state-eq :closed))
(def locked? (partial barrier-state-eq :locked))

(defn can-open?
  [barrier-ent]
  (closed? barrier-ent))

(defn can-close?
  [barrier-ent]
  (open? barrier-ent))

(defn- set-state
  [state ent-id]
  (e/set-state ent-id :state state))

(def set-open (partial set-state :open))
(def set-closed (partial set-state :closed))
(def set-locked (partial set-state :locked))
