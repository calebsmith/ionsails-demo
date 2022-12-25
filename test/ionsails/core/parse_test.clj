(ns ionsails.core.parse-test
  (:require [ionsails.core.parse :as sut]
            [clojure.test :as t :refer [is testing deftest]]))

(deftest parse-command
  (testing "parse"
  ;; No args
    (is (= ["get" []] (sut/parse-command "get")))
    (is (= ["get" [{:eid 1}]] (sut/parse-command "get @1")))
    (is (= ["get" [{:eid 1 :quantity 2}]] (sut/parse-command "get 2 @1")))
    (is (= ["get" [{:keywords #{"bag"}}]] (sut/parse-command "get bag")))
    (is (= ["get" [{:modifier :all}]] (sut/parse-command "get all")))
    (is (= ["get" [{:modifier :each :keywords #{"bag"}}]] (sut/parse-command "get each bag")))
    (is (= ["get" [{:quantity 2 :keywords #{"bag"}}]] (sut/parse-command "get 2 bag")))
    (is (= ["get" [{:keywords #{"paper" "bag"}}]] (sut/parse-command "get \"paper bag\"")))
    (is (= ["get" [{:keywords #{"paper" "bag"} :quantity 2}]] (sut/parse-command "get 2 \"paper bag\"")))
    (is (= ["get" [{:keywords #{"wrench"} :rank 1}]] (sut/parse-command "get wrench #1")))
    (is (= ["get" [{:keywords #{"bag"}} {:modifier :in :keywords #{"backpack"}}]] (sut/parse-command "get bag in backpack")))
    (is (= ["get" [{:modifier :each :keywords #{"bag"}} {:modifier :in :keywords #{"backpack"}}]] (sut/parse-command "get each bag in backpack")))
    (is (= ["get" [{:keywords #{"wrench"}} {:modifier :in :keywords #{"bag"}} {:modifier :in :keywords #{"backpack"}}]] (sut/parse-command "get wrench in bag in backpack")))
    (is (= ["pour" [{:quantity 2 :rank 3 :keywords #{"bottle"}} {:keywords #{"glass"}}]] (sut/parse-command "pour 2 bottle #3 glass")))))
