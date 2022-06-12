(ns ionsails.core.queriest-test
  (:require [ionsails.core.queries :as sut]
            [ionsails.core.data :as dat]
            [clojure.test :as t :refer [deftest testing is]]
            [datascript.core :as d]))

(defn db-setup [test-func]
  (dat/init)
  (test-func)
  (d/reset-conn! (dat/get-conn) (d/empty-db dat/schema)))

(t/use-fixtures :once db-setup)

(defn get-title-from-eid [eid]
  (:title (sut/item-details (dat/get-db) eid)))

(deftest base-queries
  (testing "base-queries"
    (let [db (dat/get-db)
          room (sut/look-room db dat/test-sender)
          room-item (sut/look-at db dat/test-sender #{"wrench"})
          inv-item-title (get-title-from-eid (sut/find-in-inventory-query db dat/test-sender #{"backpack"}))
          container-list (:contents (sut/query-in-container db dat/test-sender #{"backpack"}))
          item-in-container (sut/query-at-item-in-container db dat/test-sender #{"backpack"} #{"adjustable"})
          ]
      (is (= "A spaceport garage" (:title room)))
      (is (= "a wrench" (:title room-item)))
      (is (= "a backpack" inv-item-title))
      (is (= ["a wrench" "an adjustable wrench"] (map :title container-list)))
      (is (= "an adjustable wrench" (:title item-in-container))))))

(deftest template-search
  (testing "template-serach "
    (let [db (dat/get-db)
          res-name (sut/template-search db dat/test-sender "pale-amber-beer")
          res-q (sut/template-search db dat/test-sender #{"pale"})]
      (is (= (sut/template-search db dat/test-sender "asdfasdf-asdf") nil))
      (is (= (sut/template-search db dat/test-sender #{"fasdfasdf" "rasdfasdf"}) nil))
      (is (= (:template-name res-name) "pale-amber-beer"))
      (is (= (mapv :template-name res-q) ["pale-amber-beer"])))))

(deftest ent-queries
  (testing "ent-queries"
    (let [db (dat/get-db)
          res (sut/find-player-room-item-in-room-query db dat/test-sender #{"wrench"})
          entmap (into {} (for [[k v] res] [k (:title (d/entity db v))]))]
      (is (= entmap {:room "A spaceport garage" :item "a wrench" :player "caleb"})))
    (let [db (dat/get-db)
          res (sut/find-player-room-items-in-room-query db dat/test-sender #{})]
      (is (= res #{{:player 9, :room 3, :item 8}
                   {:player 9, :room 3, :item 7}
                   {:player 9, :room 3, :item 10}
                   {:player 9, :room 3, :item 9}})))
    #_(let [db (dat/get-db)
          res (sut/find-player-room-item-in-inv-query db dat/test-sender #{"backpack"})
          d (sut/items-details db [(:item res)])])
    #_(let [db (dat/get-db)
            res (sut/find-player-room-item-in-room-query db dat/test-sender #{})
            res2 (sut/find-player-room-item-in-room-query db dat/test-sender nil)]
        (is (= {:player nil :room nil :item nil} res)
            (= {:player nil :room nil :item nil} res2)))))

(deftest timers
  (testing "tick"
    (let [db (dat/get-db)
          res (sut/get-tick db)]
      (is (= [1 0] res)))
    (testing "timers"
      (let [db (dat/get-db)
            res (sut/get-timers db 1)]
        (is (=
             {:consume-fuel
              [{:db/id 27,
                :tick-action :consume-fuel,
                :parent 4
                :tick-next 1,
                :tick-recur 2}]}
             res))))))
