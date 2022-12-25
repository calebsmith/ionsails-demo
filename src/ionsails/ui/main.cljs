(ns ionsails.ui.main
  (:require
   [reagent.dom :as rdom]
   [ionsails.ui.views.main :as main-view]
   [ionsails.ui.data.main :as data]
   [ionsails.core.data :as cdata]
   [ionsails.ui.pubsub :as pubsub]
   [ionsails.ui.timers :as timers]
   [ionsails.ui.config :as config]
   ;; Required for top-level effects
   [ionsails.core.commands]
   [ionsails.ui.listeners]))

(defn dev-setup []
  (when config/debug?
    (println "dev mode")))

(defn ^:dev/after-load mount-root []
  (let [root-el (.getElementById js/document "app")]
    (rdom/unmount-component-at-node root-el)
    (rdom/render [main-view/main] root-el)))

;; Hardcoded player account name for now
(def owner-id "caleb@example.com caleb")

(defn init []
  (dev-setup)
  (data/init owner-id)
  (cdata/init)
  (pubsub/initialize)
  (mount-root)
  (timers/start!)
  (pubsub/send :player-message-receive {:messages [{:category :echo :text "Welcome new player! Type help and press enter for help."}]})
  (pubsub/send :player-command {:message "look"}))

(defn^ :dev/after-load start []
  (pubsub/reset-listeners))
