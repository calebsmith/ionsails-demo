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
    [(entity-inventory ?ent ?ent-inv)
     [?ent :contents ?ent-inv]]
    [(player-equipment ?owner ?owner-eid ?eq)
     [?owner-eid :owner ?owner]
     [?owner-eid :equips ?eq]]
    [(find-adjacent-location ?src-loc ?direction ?target-loc ?barrier)
     [?src-loc :exits ?exit]
     [?exit :direction ?direction]
     [?exit :location ?target-loc]
     [(get-else $ ?exit :barrier :none) ?barrier]]
    [(filter-by-eid ?eid ?ent)
     [(= ?ent ?eid)]]
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

(defn- find-in-scope-query
  [[scope & rest-scopes] db owner arg]
  (let [{:keys [keywords eid rank]} arg
        rank (or rank 999)
        res (d/q {:find '[?owner-eid ?loc ?item]
                  :keys '[player room item]
                  :in '[$ % ?owner ?kw-in ?eid subset?]
                  :where
                  (concat
                   ['(player-location ?owner ?owner-eid ?loc)]
                   (condp = scope
                     :player-inventory ['(player-inventory ?owner ?owner-eid ?item)]
                     :player-equipment ['(player-equipment ?owner ?owner-eid ?item)]
                     :player-room ['(entity-inventory ?loc ?item)]
                     :player-inventory-vessels ['(player-inventory ?owner ?owner-eid ?item)
                                                '[?item :holds-liquid? true]])
                   (if eid
                     ['(filter-by-eid ?eid ?item)]
                     ['(filter-by-kws ?kw-in ?item)]))
                  :limit rank}
                 db rules owner keywords eid clojure.set/subset?)]
    (if (or (seq res)
            (empty? rest-scopes))
      res
      (recur rest-scopes db owner arg))))

(def qs-find-in-player-inv (partial find-in-scope-query [:player-inventory]))
(def qs-find-in-player-room (partial find-in-scope-query [:player-room]))
(def qs-find-in-player-inv-or-room (partial find-in-scope-query [:player-inventory :player-room]))
(def qs-find-in-player-eq (partial find-in-scope-query [:player-equipment]))
(def qs-find-in-player-inv-vessels (partial find-in-scope-query [:player-inventory-vessels]))

(defn apply-select-to
  "Chooses a single candidate from a queryset using rank if given."
  [fun db owner arg]
  (let [{:keys [eid rank]} arg]
    (if eid
      (first (fun db owner arg))
      (select-by-rank
       (fun db owner arg)
       rank))))

(def q-find-in-player-inv (partial apply-select-to qs-find-in-player-inv))
(def q-find-in-player-room (partial apply-select-to qs-find-in-player-room))
(def q-find-in-player-inv-or-room (partial apply-select-to qs-find-in-player-inv-or-room))
(def q-find-in-player-eq (partial apply-select-to qs-find-in-player-eq))
(def q-find-in-player-inv-vessels (partial apply-select-to qs-find-in-player-inv-vessels))


(def q-find-item-in-player-inv (comp :item q-find-in-player-inv ))
(def q-find-item-in-player-inv-or-room (comp :item q-find-in-player-inv-or-room ))

(defn q-find-item-container-in-player-inv-or-room
  [db owner container-arg item-arg]
  (let [{player :player container :item} (q-find-in-player-inv-or-room db owner container-arg)
        {:keys [keywords eid rank]} item-arg
        q-res (d/q {:find '[?item]
                    :in '[$ % ?container ?inner-kw-in ?eid subset?]
                    :where
                    ['(entity-inventory ?container ?item)
                     (if eid
                       '(filter-by-eid ?eid ?item)
                       '(filter-by-kws ?inner-kw-in ?item))]
                    :limit rank}
                   db rules container keywords eid clojure.set/subset?)
        item (first (if eid
                      (first q-res)
                      (select-by-rank q-res rank)))]
    {:player player
     :container container
     :item item}))

(defn query-in-container
  [db owner arg]
  (when-let [container (q-find-item-in-player-inv-or-room db owner arg)]
    (d/pull db [:* {:contents [:title]}] container)))

(defn query-at-item-in-container
  [db owner container-arg item-arg]
  (let [ents (q-find-item-container-in-player-inv-or-room db owner container-arg item-arg)
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
  [db owner arg]
  (when-let [v (q-find-item-in-player-inv-or-room db owner arg)]
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
