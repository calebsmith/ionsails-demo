(ns ionsails.ui.listeners
  (:require [ionsails.ui.pubsub :refer [send] :refer-macros [deflistener]]
            [ionsails.ui.data.main :refer [get-owner]]
            [ionsails.core.run :as r]))

(deflistener command-receive :player-command
  [data]
  (let [sender (get-owner)
        messages (r/run-command! sender (:message data))]
    (doseq [message messages]
      (let [{:keys [recipients body]} message]
        (when (contains? recipients sender)
          (send :player-message-receive {:messages body}))))))
