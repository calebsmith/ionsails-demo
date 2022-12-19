(ns ionsails.ui.timers
  (:require [ionsails.core.tick :as t]
            [ionsails.ui.pubsub :refer [send] :refer-macros [deflistener]]))

(defn run-tick-handler!
  []
  (let [messages (t/run-tick!)]
    (doseq [message messages]
      (let [{:keys [body]} message]
        (send :player-message-receive {:messages body})))))

(defn start!
  []
  ;; FIXME: Just using 2s for now. Make a config
  (js/setInterval run-tick-handler! 2000))
