(ns ionsails.core.messages
  (:require [ionsails.core.defcommand  :refer [*sender*]]))

(defn dm [body]
  {:messages [{:recipients #{*sender*} :body body}]})

(defn msg [lvl-kw & txt]
  {:category lvl-kw :text (apply str txt)})

(def title (partial msg :title))
(def exit (partial msg :exit))
(def item (partial msg :item))
(def info (partial msg :info))
(def warn (partial msg :warning))
(def err (partial msg :error))
(def edn (partial msg :edn))

(defn dm-msg-lvl [lvl-fn & txt]
  (dm [(apply lvl-fn txt)]))

(def dm-info (partial dm-msg-lvl info))
(def dm-warn (partial dm-msg-lvl warn))
(def dm-err (partial dm-msg-lvl err))
