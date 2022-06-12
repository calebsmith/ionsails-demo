(ns ionsails.core.transactions
  (:require [datascript.core :as d]))

(declare derive-tx-data)

(defn run-transaction!
  [conn tx-data]
  (d/transact conn tx-data))

(defn db-action
  [db-ops-fn data]
  (when data
    (if (sequential? data)
      (vec (mapcat db-ops-fn data))
      (db-ops-fn data))))

(defn move->db-ops
  [source-attr target-attr {:keys [source target entity]}]
  [[:db/retract source source-attr entity]
   [:db/add target target-attr entity]])

(def move-contents->db-ops (partial move->db-ops :contents :contents))

(defn upsert->db-ops
  [{:keys [entity attribute value]}]
  [[:db/add entity attribute value]])

(defn retract->db-ops
  [{:keys [entity attribute value]}]
  [[:db/retract entity attribute value]])

(defn delete-attr->db-ops
  [{:keys [entity attribute]}]
   [[:db/retract entity attribute]])

(defn delete->db-ops
  [{:keys [entity]}]
   [[:db/retractEntity entity]])

;; FIXME: deprecated
(def move-contents (partial db-action move-contents->db-ops))

(def upsert-db (partial db-action upsert->db-ops))
(def retract-db (partial db-action retract->db-ops))
(def delete-attr-db (partial db-action delete-attr->db-ops))
(def delete-db (partial db-action delete->db-ops))

(defn derive-tx-data
  [action]
  (let [{:keys [name args]} action]
    (condp = name
      :move-contents (move-contents args)
      :create [args]
      :upsert (upsert-db args)
      :retract (retract-db args)
      :delete-attr (delete-attr-db args)
      :delete (delete-db args)
      nil)))

;; Helpers for effects to construct operations

(defn new-id
  []
  (d/tempid :db.part/user))

(defn create
  [ent & [f]]
  (let [res {:name :create :args ent}]
    (if f
      (assoc res :after-tx
              (fn [db tempids-map]
                (when-let [value (get tempids-map (:db/id ent))]
                  (f db value))))
      res)))

(defn upsert
  [ent attr v]
  {:name :upsert
   :args [{:entity ent
           :attribute attr
           :value v}]})

(defn upserts
  [pos-args]
  (let [args (mapv
              (fn [[entity attribute value]]
                {:entity entity
                 :attribute attribute
                 :value value})
              pos-args)]
    {:name :upsert
     :args args}))

(defn delete
  [ent-id]
  {:name :delete
   :args {:entity ent-id}})

(defn retract
  [ent attr v]
  {:name :retract
   :args {:entity ent
           :attribute attr
           :value v}})

(comment
  (move-contents [{:source 1 :target 1 :entity 2}
                  {:source 1 :target 1 :entity 1}])

  )
