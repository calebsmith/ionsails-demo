(ns ionsails.core.queries
  (:require [datascript.core :as d]
            [clojure.set :as set]))

(def rules
  '[[(player ?owner-eid ?owner)
     [?owner-eid :owner ?owner]]
    [(player-location ?owner ?owner-eid ?loc)
     [?owner-eid :owner ?owner]
     [?loc :contents ?owner-eid]]
    [(player-inventory ?owner ?owner-eid ?inv)
     [?owner-eid :owner ?owner]
     [?owner-eid :contents ?inv]]
    [(location-inventory ?loc ?loc-inv)
     [?loc :contents ?loc-inv]]
    [(player-equipment ?owner ?owner-eid ?eq)
     [?owner-eid :owner ?owner]
     [?owner-eid :equips ?eq]]
    [(find-adjacent-location ?src-loc ?direction ?target-loc ?barrier)
     [?src-loc :exits ?exit]
     [?exit :direction ?direction]
     [?exit :location ?target-loc]
     [(get-else $ ?exit :barrier :none) ?barrier ]]
    [(filter-by-kws ?kws-in ?ent)
     [?ent :keywords ?ent-kws]
     [(subset? ?kws-in ?ent-kws)]]])

(defn- select-by-rank
  [results rank]
  (get (vec results)
       (dec rank)))

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
            :where (player-location ?owner ?owner-eid ?loc)]
          db rules owner)))

(defn find-player-room-items-in-inv-query
  ([db owner keywords]
   (find-player-room-items-in-inv-query db owner keywords 200))
  ([db owner keywords limit]
   (d/q '[:find ?owner-eid ?loc ?inv
          :keys player room item
          :in $ % ?owner ?kw-in subset?
          :where
          (player-location ?owner ?owner-eid ?loc)
          (player-inventory ?owner ?owner-eid ?inv)
          (filter-by-kws ?kw-in ?inv)
          :limit limit]
        db rules owner keywords clojure.set/subset?)))

(defn find-player-room-item-in-inv-query
  [db owner keywords rank]
  (select-by-rank
    (find-player-room-items-in-inv-query db owner keywords rank)
    rank))

(defn find-player-room-items-in-equips-query
  ([db owner keywords]
   (find-player-room-items-in-equips-query db owner keywords 200))
  ([db owner keywords limit]
   (d/q '[:find ?owner-eid ?loc ?player-eq
          :keys player room item
          :in $ % ?owner ?kw-in subset?
          :where
          (player-location ?owner ?owner-eid ?loc)
          (player-equipment ?owner ?owner-eid ?player-eq)
          (filter-by-kws ?kw-in ?player-eq)
          :limit limit]
        db rules owner keywords clojure.set/subset?)))

(defn find-player-room-item-in-equips-query
  [db owner keywords rank]
  (select-by-rank
    (find-player-room-items-in-equips-query db owner keywords rank)
    rank))

(defn find-player-room-items-in-room-query
  ([db owner keywords]
   (find-player-room-items-in-room-query db owner keywords 999))
  ([db owner keywords limit]
   (d/q '[:find ?owner-eid ?loc ?loc-inv
          :keys player room item
          :in $ % ?owner ?kw-in subset?
          :where
          (player-location ?owner ?owner-eid ?loc)
          (location-inventory ?loc ?loc-inv)
          (filter-by-kws ?kw-in ?loc-inv)
          :limit limit]
        db rules owner keywords clojure.set/subset?)))

(defn find-player-room-item-in-room-query
  [db owner keywords rank]
  (select-by-rank
    (find-player-room-items-in-room-query db owner keywords rank)
    rank))

(defn find-in-room-query
  [db owner keywords rank]
  (first
   (select-by-rank
    (d/q '[:find ?loc-inv
           :in $ % ?owner ?kw-in subset?
           :where
           (player-location ?owner ?owner-eid ?loc)
           (location-inventory ?loc ?loc-inv)
           (filter-by-kws ?kw-in ?loc-inv)
           :limit rank]
         db rules owner keywords clojure.set/subset?)
    rank)))

(defn find-in-inventory-query
  [db owner keywords rank]
  (first
   (select-by-rank
    (d/q '[:find ?inv
           :in $ % ?owner ?kw-in subset?
           :where
           (player-inventory ?owner ?owner-id ?inv)
           (filter-by-kws ?kw-in ?inv)
           :limit rank]
         db rules owner keywords clojure.set/subset?)
    rank)))

;; FIXME: Rewrite this to use rank/limit

(defn find-in-container-query
  [db owner container-kws container-rank inner-kws inner-rank]
  (d/q '[:find [?owner-eid ?inv ?inside]
         :keys player container item
         :in $ % ?owner ?container-kw-in ?inner-kw-in subset?
         :where
         (player-inventory ?owner ?owner-eid ?inv)
         (filter-by-kws ?container-kw-in ?inv)
         [?inv :contents ?inside]
         (filter-by-kws ?inner-kw-in ?inside)]
       db rules owner container-kws inner-kws clojure.set/subset?))

(defn find-vessel-query
  [db owner kws rank]
  (select-by-rank
   (d/q '[:find ?owner-eid ?inv
          :keys player item
          :in $ % ?owner ?kws-in subset?
          :where
          (player-inventory ?owner ?owner-eid ?inv)
          [?inv :holds-liquid? true]
          (filter-by-kws ?kws-in ?inv)
          :limit rank]
        db rules owner kws clojure.set/subset?)
   rank))

(defn find-in-room-or-inventory-query
  [db owner keywords rank]
  (or (find-in-room-query db owner keywords rank)
      (find-in-inventory-query db owner keywords rank)))

(defn find-in-inventory-or-room-query
  [db owner keywords rank]
  (or (find-in-inventory-query db owner keywords rank)
      (find-in-room-query db owner keywords rank)))

(defn query-in-container
  [db owner container-kws rank]
  (when-let [container (find-in-room-or-inventory-query db owner container-kws rank)]
    (d/pull db [:* {:contents [:title]}] container)))

(defn query-at-item-in-container
  [db owner container-kws container-rank inner-kws inner-rank]
  (let [ents (find-in-container-query db owner container-kws container-rank inner-kws inner-rank)
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
  [db owner keywords rank]
  (when-let [v (find-in-room-or-inventory-query db owner keywords rank)]
    (d/pull db [:* {:contents [:title]}] v)))

(def barrier-pull [:*])

(defn move-room-eids
  [db owner direction]
  (let [move-res (d/q '[:find [?loc ?target-loc ?owner-eid ?barrier]
                        :keys source target entity barrier
                        :in $ % ?owner ?direction
                        :where
                        (player-location ?owner ?owner-eid ?loc)
                        (find-adjacent-location ?loc ?direction ?target-loc ?barrier)]
                      db rules owner direction)]
    (update move-res :barrier
            (fn [barrier]
              (if (or (= barrier :none) (nil? barrier))
                nil
                (d/pull db barrier-pull barrier))))))

;;; Timers ;;


(defn get-tick
  [db]
  (:tick (d/pull db [:tick] [:counter :global])))

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
