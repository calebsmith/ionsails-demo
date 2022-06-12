(ns ionsails.ui.timers
  (:require [ionsails.core.tick :as t]))

(defn start!
  []
  ;; FIXME: Just using 2s for now. Make a config
  (js/setInterval t/run-tick! 2000))
