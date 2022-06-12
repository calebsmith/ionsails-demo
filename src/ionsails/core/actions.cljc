(ns ionsails.core.actions
  (:require [ionsails.core.transactions :as t]))

(defn resolve-effects!
  [db effects]
  (if (fn? effects)
    (resolve-effects! db (effects db))
    (if (or (vector? effects) (seq? effects))
      effects
      [effects])))

(defn apply-effects!
  [conn db sender effects]
  ;;(prn "effects:" effects)
  (let [tx-actions (resolve-effects! db effects)
        ;;_ (prn "resolved effects:" tx-actions)
        tx-datas (vec (mapcat t/derive-tx-data tx-actions))
        ;;_ (prn "tx-datas:" tx-datas)
        tx @(t/run-transaction! conn tx-datas)]
    (doall
     (map (fn [{:keys [after-tx]}]
            (when after-tx
              (let [tempids (:tempids tx)
                    after-db (:after-db tx)
                    next-effects (after-tx after-db tempids)]
                (when next-effects
                  (apply-effects! conn after-db sender next-effects)))))
          tx-actions))))
