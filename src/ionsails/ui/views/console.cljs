(ns ionsails.ui.views.console
  (:require [reagent.core :as r]
            [reagent.dom :as rd]
            [ionsails.ui.pubsub :refer [send] :refer-macros [deflistener]]
            [ionsails.ui.data.console :as data :refer [main-console]]))

(deflistener command-result :player-command-result
  [{:keys [messages]} data]
  (data/append-messages! messages))

;; --- Keypress handlers for command line input --- ;;

;; FIXME: Implement dropdown for multiple suggestions
(defn handle-tab
  [evt val]
  (.preventDefault evt)
  (data/auto-complete! evt val))

(defn handle-enter
  [evt val]
  (data/submit! evt val)
  ;; TODO: Check if echo on is set.
  (data/append-messages! [{:category :input :text (str "=> " val)}] )
  (send :player-command {:message val}))

(defn handle-up [evt val]
  (.preventDefault evt)
  (data/history-previous! evt val))

(def handle-down data/history-next!)
(def handle-nop identity)

(def handler-lookup
  {"Tab" handle-tab
   "Enter" handle-enter
   "ArrowUp" handle-up
   "ArrowDown" handle-down})

(defn on-command-line-keydown
  [evt]
  (let [keyPressName (.-key evt)
        inputValue (.-value (.-target evt))
        handler (get handler-lookup keyPressName handle-nop)]
    (handler evt inputValue)))

(defn on-command-line-change
  [evt]
  (let [inputValue (.-value (.-target evt))]
    (data/set-input! evt inputValue)))

(defn command-line-input
  [input-value]
  [:div.command-line [:input {:autoFocus true
                              :value input-value
                              :onChange on-command-line-change
                              :onKeyDown on-command-line-keydown}]])

(defn scroll-bottom
  "When console is not in focus, force scroll to the bottom"
  [comp]
  (let [node (rd/dom-node comp)]
    (when-not (.matches node ":focus")
      (set! (.-scrollTop node) (.-scrollHeight node)))))

(defn console-output-panel
  [messages]
  (r/create-class
   {:display-name "ConsoleOutputPanel"
    :component-did-mount scroll-bottom
    :component-did-update scroll-bottom
    :reagent-render
    (fn [messages]
      [:div.console
       [:ul
        (for [[i msg] (map-indexed vector messages)]
          (let [{:keys [text category]} msg]
            ^{:key i}
            [:li {:class category} text]))]])}))

(defn main-panel []
  (let [console-data @main-console
        {:keys [messages, input-value]} console-data]
    [:div
     [console-output-panel messages]
     [command-line-input input-value]]))

(comment

  (data/append-messages! [{:category :info, :text "YO"}])

  )
