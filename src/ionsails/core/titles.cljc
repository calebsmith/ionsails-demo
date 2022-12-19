(ns ionsails.core.titles
  (:require [clojure.string :as str]
            [ionsails.core.messages :refer [item info] :as m]
            [ionsails.core.systems.quantifiable :as quant]
            [ionsails.core.systems.flammable :as flam])
  #?(:clj (:require [clojure.pprint :as pprint]))
  #?(:cljs (:require [cljs.pprint :as pprint])))

(declare get-title)
(declare get-description)

(defn get-liquids-title
  [ents]
  (let [liquids-count (count ents)
        liquid-titles (map :title ents)]
    (if (< liquids-count 2)
      (first liquid-titles)
      (str
       (str/join ", " (butlast liquid-titles))
       " and "
       (last liquid-titles)))))

(defn get-vessel-title
  [ent]
  (let [vessel-title (:title ent)
        liquids (quant/get-liquids ent)
        liquids-count (count liquids)
        liquids-str (get-liquids-title liquids)]
    (if (> liquids-count 0)
      (str vessel-title " of " liquids-str)
      vessel-title)))

(defn get-liquid-title
  [ent]
  (let [title (:title ent)]
    (str "a puddle of " title)))

(defn get-edn
  [ent]
  (with-out-str (-> ent
                    pprint/pprint)))

(defn get-title
  [ent]
  (cond
    (:template-name ent) (get-edn ent)
    (:holds-liquid? ent) (get-vessel-title ent)
    (:liquid? ent) (get-liquid-title ent)
    :else (:title ent)))

(defn get-title-no-article
  [ent]
  (let [title (get-title ent)
        [_ & rem-title] (str/split title #" ")]
    (str/join " " rem-title)))

(defn get-container-description
  [ent]
  (if (:holds-liquid? ent)
    ;; TBI: These two are similar for now but may want to show quantities of liquids
    (let [container-title (:title ent)
          container-desc (str container-title " contains:")
          container-desc-msg (info container-desc)
          inner-descs (mapv (fn [e] (item (get-title e))) (:contents ent))]
      (if (empty? inner-descs)
        [(info container-title " is empty")]
        (concat [container-desc-msg] inner-descs)))
    (let [container-title (get-title ent)
          container-desc (str container-title " contains:")
          container-desc-msg (info container-desc)
          inner-descs (mapv (fn [e] (item (get-title e))) (:contents ent))]
      (if (empty? inner-descs)
        [(info container-title " is empty")]
        (concat [container-desc-msg] inner-descs)))))

(defn general-description
  [ent]
  (let [{:keys [can-hold? description health]} ent
        headline-category (if can-hold? :item :title)]
    (concat
     [(m/msg headline-category "You see " description ".")]
    ;; TODO: Create health description lookup
     )))

(defn get-description
  [ent]
  (cond
    (:contents ent) (get-container-description ent)
    :else (general-description ent)))
