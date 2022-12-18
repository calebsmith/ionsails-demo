(ns ionsails.core.queries
  (:require [datascript.core :as d]
            [clojure.set :as set]))

(def rules
  '[[(player-location ?loc ?owner)
     [?o :owner ?owner]
     [?loc :contents ?o]]
    [(player ?o ?owner)
     [?o :owner ?owner]]
    [(player-inventory ?inv ?owner)
     [?owner-eid :owner ?owner]
     [?owner-eid :contents ?inv]]
    [(player-equipment ?eq ?owner)
     [?owner-eid :owner ?owner]
     [?owner-eid :equips ?eq]]
    [(find-adjacent-location ?src-loc ?direction ?target-loc)
     [?src-loc :exits ?exit]
     [?exit :direction ?direction]
     [?exit :location ?target-loc]]
    [(filter-by-kws ?kws-in ?ent)
     [?ent :keywords ?ent-kws]
     [(subset? ?kws-in ?ent-kws)]]])

(defn find-by-eid
  [db owner eid & [pull-patt]]
  (let [pull-patt (or pull-patt [:*])]
    (d/pull db pull-patt eid)))

(defn template-search
  [db owner kws-or-name & [pull-patt]]
  (let [pull-patt (or pull-patt [:*])]
    (d/q '[:find [(pull ?e ?pull-patt)]
           :in $ % ?owner ?pull-patt ?kws-in subset?
           :where
           [?e :template-name _]
           (filter-by-kws ?kws-in ?e)]
         db rules owner pull-patt kws-or-name clojure.set/subset?)))

(defn player-query
  [db owner]
  (first
   (d/q '[:find [?player-eid]
          :in $ % ?owner
          :where (player ?player-eid ?owner)]
        db rules owner)))

(defn look-room-query
  [db owner]
    (first
     (d/q '[:find [?loc]
            :in $ % ?owner
            :where (player-location ?loc ?owner)]
          db rules owner)))

(defn find-player-room-item-in-inv-query
  [db owner keywords]
  (d/q '[:find [?owner-eid ?loc ?player-c]
         :keys player room item
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?loc :contents ?owner-eid]
         [?owner-eid :contents ?player-c]
         (filter-by-kws ?kw-in ?player-c)]
       db rules owner keywords clojure.set/subset?))

(defn find-player-room-items-in-inv-query
  [db owner keywords]
  (d/q '[:find ?owner-eid ?loc ?player-c
         :keys player room item
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?loc :contents ?owner-eid]
         [?owner-eid :contents ?player-c]
         (filter-by-kws ?kw-in ?player-c)]
       db rules owner keywords clojure.set/subset?))

(defn find-player-room-items-in-equips-query
  [db owner keywords]
  (d/q '[:find ?owner-eid ?loc ?player-e
         :keys player room item
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?loc :contents ?owner-eid]
         [?owner-eid :equips ?player-e]
         (filter-by-kws ?kw-in ?player-e)]
       db rules owner keywords clojure.set/subset?))

(defn find-player-room-item-in-equips-query
  [db owner keywords]
  (d/q '[:find [?owner-eid ?loc ?player-e]
         :keys player room item
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?loc :contents ?owner-eid]
         [?owner-eid :equips ?player-e]
         (filter-by-kws ?kw-in ?player-e)]
       db rules owner keywords clojure.set/subset?))

(defn find-player-room-item-in-room-query
  [db owner keywords]
  (d/q '[:find [?owner-eid ?loc ?loc-c]
         :keys player room item
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?loc :contents ?owner-eid]
         [?loc :contents ?loc-c]
         (filter-by-kws ?kw-in ?loc-c)]
       db rules owner keywords clojure.set/subset?))

;; TODO refactor to share with find-player-room-item-in-room-query
(defn find-player-room-items-in-room-query
  [db owner keywords]
  (d/q '[:find ?owner-eid ?loc ?loc-c
         :keys player room item
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?loc :contents ?owner-eid]
         [?loc :contents ?loc-c]
         (filter-by-kws ?kw-in ?loc-c)]
       db rules owner keywords clojure.set/subset?))

(defn find-in-room-query
  [db owner keywords]
  (first (d/q '[:find [?loc-c]
                :in $ % ?owner ?kw-in subset?
                :where
                [?owner-eid :owner ?owner]
                [?loc :contents ?owner-eid]
                [?loc :contents ?loc-c]
                (filter-by-kws ?kw-in ?loc-c)]
              db rules owner keywords clojure.set/subset?)))

(defn find-in-inventory-query
  [db owner keywords]
  (first (d/q '[:find [?player-c]
                :in $ % ?owner ?kw-in subset?
                :where
                ;; use (player-inventory  ?
                [?owner-eid :owner ?owner]
                [?owner-eid :contents ?player-c]
                (filter-by-kws ?kw-in ?player-c)]
              db rules owner keywords clojure.set/subset?)))

(defn find-in-container-query
  [db owner container-kws inner-kws]
   (d/q '[:find [?owner-eid ?player-c ?inside]
          :keys player container item
          :in $ % ?owner ?container-kw-in ?inner-kw-in subset?
          :where
          [?owner-eid :owner ?owner]
          [?owner-eid :contents ?player-c]
          (filter-by-kws ?container-kw-in ?player-c)
          [?player-c :contents ?inside]
          (filter-by-kws ?inner-kw-in ?inside)]
        db rules owner container-kws inner-kws clojure.set/subset?))

(defn find-vessel-query
  [db owner kws]
   (d/q '[:find [?owner-eid ?player-c]
          :keys player item
          :in $ % ?owner ?kws-in subset?
          :where
          [?owner-eid :owner ?owner]
          [?owner-eid :contents ?player-c]
          [?player-c :holds-liquid? true]
          (filter-by-kws ?kws-in ?player-c)]
        db rules owner kws clojure.set/subset?))

(defn find-in-vessel-query
  [db owner keywords]
  (d/q '[:find [?owner-eid ?player-c]
         :keys player container
         :in $ % ?owner ?kw-in subset?
         :where
         [?owner-eid :owner ?owner]
         [?owner-eid :contents ?player-c]
         [?player-c :holds-liquid? true]
         [?player-c :contents ?inside]
         (or
          (filter-by-kws ?kw-in ?player-c)
          (and
            [?inside :liquid? true]
            (filter-by-kws ?kw-in ?inside)))]
       db rules owner keywords clojure.set/subset?))

(defn find-in-room-or-inventory-query
  [db owner keywords]
  (or (find-in-room-query db owner keywords)
      (find-in-inventory-query db owner keywords)))

(defn find-in-inventory-or-room-query
  [db owner keywords]
  (or (find-in-inventory-query db owner keywords)
      (find-in-room-query db owner keywords)))

(defn query-in-container
  [db owner container-kws]
  (when-let [container (find-in-room-or-inventory-query db owner container-kws)]
    (d/pull db [:* {:contents [:title]}] container)))

(defn query-at-item-in-container
  [db owner container-kws inner-kws]
  (let [ents (find-in-container-query db owner container-kws inner-kws)
        item (:item ents)]
    (when item
      (d/pull db [:*] item))))

(def look-room-pull
  [:*
   {:exits [:direction]
    :contents [:title :owner :can-hold? :can-transport? :holds-liquid? :liquid?
               {:contents [:title]}]}])

(defn look-room
  [db owner]
    (d/pull db  look-room-pull (look-room-query db owner)))

(def item-pull [:* {:contents [:title]
                    :equips [:title]}])

(defn item-details [db item] (d/pull db item-pull item))
(defn items-details [db items] (d/pull-many db item-pull items))

(def vessel-pull [:* {:contents [:*] :timers [:*]}])
(defn vessel-details [db item] (d/pull db vessel-pull item))

(def player-inventory-pull
  [{:equips [:* {:contents [:title] :equips [:title]}]
    :contents [:* {:contents [:title]}]}])

(def player-equips-pull
  [{:equips [:* {:contents [:title] :equips [:title]}]}])

(defn player-equipment
  [db owner]
  (:equips
   (d/pull db player-equips-pull (player-query db owner))))

(defn player-inventory
  [db owner]
  (:contents
   (d/pull db player-inventory-pull (player-query db owner))))

(defn player-inventory-equips
  [db owner]
  (d/pull db (merge player-inventory-pull player-equips-pull) (player-query db owner)))

(defn look-at
  [db owner keywords]
  (when-let [v (find-in-room-or-inventory-query db owner keywords)]
    (d/pull db [:* {:contents [:title]}] v)))

(defn move-room-eids
  [db owner direction]
  (let [query-results
        (d/q '[:find [?loc ?target-loc ?owner-eid]
               :in $ % ?owner ?direction
               :where
               (player-location ?loc ?owner)
               (find-adjacent-location ?loc ?direction ?target-loc)
               [?owner-eid :owner ?owner]]
             db rules owner direction)
        [src-loc-eid target-loc-eid owner-eid] query-results]
    (when (not (or (nil? query-results) (empty? query-results)))
      {:source src-loc-eid,
       :target target-loc-eid,
       :entity owner-eid})))


;;; Timers ;;


(defn get-tick
  [db]
  (d/q '[:find [?e ?v]
         :keys id value
         :in $
         :where [?e :tick ?v]]
       db))

(defn get-timers
  [db tick-value]
  (let [timer-data (d/q '[:find [(pull ?e [:* :_timers])]
                          :in $ ?tick-value
                          :where [?e :tick-next ?tick-value]]
                        db tick-value)
        set-parent (fn [td]
                     (let [parent (-> td :_timers :db/id)]
                       (-> td
                           (assoc :parent parent)
                           (dissoc :_timers))))]
    (->> timer-data
         (mapv set-parent)
         (group-by :tick-action))))
