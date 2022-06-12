(ns ionsails.ui.data.main
  (:require [reagent.core :as r]
            [ionsails.core.data :as core-data]
            [ionsails.util.trie :as trie]
            [ionsails.core.defcommand :as dc]
            [amalloy.ring-buffer :as buff]))

(defonce world (r/atom {}))

(defn get-owner [] (:owner-id @world))

(defn init [owner-id]
  (reset! world {:main-console {;; FIXME: Temprorary until ring-buffer is used
                                :messages (buff/ring-buffer 1000)
                                :completions (trie/build-trie (dc/cmd-list))
                                :input-value ""
                                :history (buff/ring-buffer 250)
                                :history-line -1}
                 :owner-id owner-id,
                 :entity-conn (core-data/get-conn)}))
