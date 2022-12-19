(ns ionsails.core.data
  (:require [datascript.core :as d]))

(def schema {:template-name {:db/unique :db.unique/identity}
             :template-id {:db/valueType :db.type/ref
                           :db/cardinality :db.cardinality/one}
             :owner {:db/unique :db.unique/identity}
             :exits {:db/valueType :db.type/ref
                     :db/cardinality :db.cardinality/many
                     :db/isComponent true}
             :location {:db/valueType :db.type/ref
                        :db/cardinality :db.cardinality/one}
             :locations {:db/valueType :db.type/ref
                         :db/cardinality :db.cardinality/many
                         :db/isComponent true}
             :contents {:db/valueType :db.type/ref
                        :db/cardinality :db.cardinality/many}
             :equips {:db/valueType :db.type/ref
                        :db/cardinality :db.cardinality/many}
             :timers {:db/valueType :db.type/ref
                      :db/cardinality :db.cardinality/many
                         :db/isComponent true}})

;; --- Temporary test data and globals --- ;;

(def test-sender "caleb@example.com caleb")

(def global-tick
  {:db/id -999999
   :tick 0})

(def area
  {:db/id -1
   :title "mars centari spaceport"
   :description "A spaceport in the Centari region of Mars"
   :keywords #{"centari" "mars" "spaceport"}
   :locations #{-2 -3}})

(def room
  {:db/id -2
   :title "A spaceport dive bar"
   :description "A jagged and rough strewn room with a long bar and some stools"
   :exits #{{:direction :east :location -3}}
   :contents #{-30}})

(def room2
  {:db/id -3
   :title "A spaceport garage"
   :description "A vast steel room for docking and reparing ships"
   :exits #{{:direction :west :location -2}}
   :contents #{-4 -6 -8 -18}})

(def ship
  {:db/id -8
   :can-transport? true
   :title "a space glider"
   :keywords #{"space" "glider" "ship"}
   :description "a small space glider"
   :contents #{}
   :capacity 10})

(def ship2
  {:db/id -18
   :can-transport? true
   :title "a space freight ship"
   :keywords #{"space" "freight" "ship"}
   :description "a large space freighter"
   :contents #{}
   :capacity 100})


(def mob
  {:db/id -7
   :title "a bartender"
   :keywords #{"bartender"}
   :description "a bartender"
   :can-free-will? true
   ;;:contents #{}
   })

(def item
  {:db/id -4
   :can-hold? true
   :title "a wrench"
   :description "a steel wrench"
   :keywords #{"steel" "wrench"}})

(def item3
  {:db/id -14
   :can-hold? true
   :title "a wrench"
   :description "a steel wrench"
   :keywords #{"steel" "wrench"}})

(def item4
  {:db/id -15
   :can-hold? true
   :title "an adjustable wrench"
   :description "an adjustable wrench"
   :keywords #{"adjustable" "wrench"}})

(def template-liquid1
  {:db/id -1000
   :template-name "pale-amber-beer"
   :liquid? true
   :quantity 100
   :title "pale amber beer"
   :description "pale amber beer"
   :keywords #{"pale" "amber" "beer"}})

(def template-liquid2
  {:db/id -1003
   :template-name "olive-oil"
   :liquid? true
   :burn-rate 4
   :quantity 2
   :title "olive oil"
   :description "olive oil"
   :keywords #{"olive" "oil"}})

(def template-clothing1
  {:db/id -1001
   :equip-location :torso
   :layers "2"
   :title "a cloth tunic"
   :description "cloth tunic"
   :keywords #{"cloth" "tunic"}})

(def template-clothing2
  {:db/id -1002
   :equip-location :legs
   :layers "2"
   :title "a pair of rugged breeches"
   :description "rugged breeches"
   :keywords #{"rugged" "breeches"}})

(def template-light
  {:db/id -1004
   :can-hold? true
   :title "a brass lantern"
   :description "a brass lantern"
   :keywords #{"lantern" "brass"}})

(def liquid1
  {:db/id -20
   :template-id -1000
   :liquid? true
   :quantity 450
   :title "pale amber beer"
   :description "pale amber beer"
   :keywords #{"pale" "amber" "beer"}})

(def liquid2
  (-> template-liquid2
      (dissoc :template-name)
      (assoc  :db/id -21
              :template-id -1003)))

(def light
  (-> template-light
      (dissoc :template-name)
      (assoc  :db/id -42
              :contents #{-21})))

(def liquid3
  (-> template-liquid2
      (dissoc :template-name)
      (assoc  :db/id -22
              :template-id -1003)))

(def light2
  (-> template-light
      (dissoc :template-name)
      (assoc  :db/id -43
              :contents #{-22})))

(def clothing1
  (-> template-clothing1
      (dissoc :template-name)
      (assoc  :db/id -40)))

(def clothing2
  (-> template-clothing2
      (dissoc :template-name)
      (assoc  :db/id -41)))

;; container
(def item2
  {:db/id -5
   :can-hold? true
   :title "a backpack"
   :description "a burlap backpack"
   :keywords #{"backpack" "burlap"}
   :contents #{-14 -15}})

(def liquid-container
  {:db/id -30
   :can-hold? true
   :holds-liquid? true
   :capacity 500
   :title "a tall glass"
   :description "a tall glass"
   :keywords #{"glass" "tall"}
   :contents #{-20}})

(def liquid-container2
  {:db/id -31
   :can-hold? true
   :holds-liquid? true
   :capacity 200
   :title "a ceramic mug"
   :description "a ceramic mug"
   :keywords #{"mug" "ceramic"}
   })

;; player
(def player
  {:db/id -6
   :owner test-sender
   :title "caleb"
   :description "a tall pale elf"
   :keywords #{"tall" "pale" "elf"}
   :equips #{-40}
   :contents #{-5 -31 -41 -42 -43}})

(def test-data
  [global-tick, player, area, room , room2,
   item, item2, item3, item4,
   mob,
   ship, ship2
template-liquid1, template-liquid2
   liquid1, liquid2, liquid3, liquid-container, liquid-container2
   template-clothing1, template-clothing2, clothing1 clothing2,
   template-light, light light2])

;; -- end temporary test data --

(defonce conn (d/create-conn schema))

;; Update schema on reload if changed
(when-not (= schema (:schema @conn))
  (set! conn (d/conn-from-datoms (d/datoms @conn :eavt) schema)))

(defn init []
  (d/transact! conn test-data))

(defn get-conn [] conn)
(defn get-db [] @conn)

(defn serialize []
  (prn-str
   (d/datoms @conn :eavt)))

;; FIXME: Implement for CLJS ?
#_(defn deserialize
    [input-str]
    (set! conn (d/conn-from-datoms (cljs-reader/read-string input-str) schema)))

(comment

  (init)

  )
