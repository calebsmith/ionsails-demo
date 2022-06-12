(ns ionsails.ui.data.console
  (:require
   [ionsails.ui.data.main :refer [world]]
   [ionsails.util.trie :as trie])
  (:require-macros [reagent.ratom :refer [reaction]]))

(def main-console (reaction (:main-console @world)))

(defn append-messages!
  [messages]
  (swap! world update-in [:main-console :messages] concat messages))

(defn set-input!
  [_ value]
  (swap! world assoc-in [:main-console :input-value] value))

(defn submit!
  [_ value]
  (swap! world update :main-console merge {:history-line -1, :input-value ""})
  (when (not= value "")
    (swap! world update-in [:main-console :history] conj value)))


;; TODO: Remove reverse and change line number math to do lookup w/o it
(defn history-previous!
  [_ _]
  (let [history-lines (vec (reverse (-> @world :main-console :history)))
        history-size (count history-lines)
        history-line-num (inc (get-in @world [:main-console :history-line] -1))
        hist-value (get history-lines history-line-num)]
    (when (and (< history-line-num history-size) hist-value)
      (swap! world update :main-console merge {:history-line history-line-num :input-value hist-value}))))


(defn history-next!
  [_ _]
  (let [history-lines (vec (reverse (-> @world :main-console :history)))
        history-size (count history-lines)
        history-line-num (dec (get-in @world [:main-console :history-line] 0))
        hist-value (get history-lines history-line-num "")]
    (when (>= history-line-num -1)
      (swap! world update :main-console merge {:history-line history-line-num :input-value hist-value}))))

;; TODO: Also handle dropdown for multiple suggestions
;; TODO: Also handle tab for completions other than top-level commands
(defn auto-complete!
  [_ value]
  (when-let [hints (trie/lookup (-> @world :main-console :completions) value)]
    (when (= (count hints) 1)
      (swap! world assoc-in [:main-console :input-value] (first hints)))))
