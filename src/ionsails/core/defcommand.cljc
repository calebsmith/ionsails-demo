(ns ionsails.core.defcommand
  (:require [ionsails.core.parse :refer [parse-command]]
            [clojure.string :as str]))

(defonce ^:private commands (atom {}))

(defn add-command
  [name docstring handler-func]
  (swap! commands assoc name {:handler handler-func :doc docstring}))

(def ^:dynamic *db*)
(def ^:dynamic *sender*)
(def ^:dynamic *args*)

(defn get-doc
  [cmd]
  (get-in @commands [(keyword cmd) :doc]))

(defn get-handler
  [cmd]
  (get-in @commands [(keyword cmd) :handler]))

(defn cmd-list
  []
  (->> @commands
       keys
       (map name)
       (remove #(str/ends-with? % "'" ))
       sort))

(defn cmd-exists?
  [cmd]
  (contains? @commands (keyword cmd)))

#?(:clj
   (defmacro defcommand
     [name docstring & body]
     (let [f `(fn [] ~@body)
           key (keyword name)]
       `(ionsails.core.defcommand/add-command ~key ~docstring ~f))))

#?(:clj
   (defmacro defalias
     [name cmd]
     (let [f `(get-handler ~cmd)
           key (keyword name)
           doc `(str "An alias for the " ~cmd " command. See - `help " ~cmd "`")]
       `(ionsails.core.defcommand/add-command ~key ~doc ~f))))

(defn invoke
  [cmd args]
  (when-let [f (get-in @commands [(keyword cmd) :handler])]
    (binding [*args* args]
      (f))))

(defn handler
  [db sender in-str]
  (let [[cmd args] (parse-command in-str)
        cmd-exists? (cmd-exists? cmd)]
    (binding [*db* db
              *sender* sender]
      (if cmd-exists?
        (invoke cmd args)
        (invoke "missing-command'" [{:message cmd}])))))
