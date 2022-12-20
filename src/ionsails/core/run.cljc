(ns ionsails.core.run
  (:require [ionsails.core.data :refer [get-conn]]
            [ionsails.core.actions :as a ]
            [ionsails.core.defcommand :refer [handler]]))

(defn run-command!
  [sender cmd-str]
  (let [conn (get-conn)
        db @conn
        res (handler db sender cmd-str)
        {:keys [messages effects force-command]} res]
    (when effects
      (a/apply-effects! conn db sender effects))
    (if force-command
      (concat messages
              (run-command! sender force-command))
      messages)))

(defn format-command-messages
  [msgs]
  (->> msgs
       (mapv
        #(let [{:keys [recipients body]} %
               body-lines (map :text body)]
           (concat recipients ["----"] body-lines)))
       flatten))

(defn run-command-print!
  [sender cmd-str]
  (let [lines (format-command-messages
               (run-command! sender cmd-str))]
    (doseq [line lines]
      (println line))))

(comment

  (run-command! "caleb@example.com caleb" "look")

  (run-command-print! "caleb@example.com caleb" "look")

  (run-command-print! "caleb@example.com caleb" "look wrench 2")

  (run-command-print! "caleb@example.com caleb" "looky")

  (run-command-print! "caleb@example.com caleb" "get wrench")

  (run-command-print! "caleb@example.com caleb" "get ship")

  (run-command-print! "caleb@example.com caleb" "inventory")

  (run-command-print! "caleb@example.com caleb" "get wrench from backpack")

  (run-command-print! "caleb@example.com caleb" "drop wrench")

  (run-command-print! "caleb@example.com caleb" "drop backpack")

  (run-command-print! "caleb@example.com caleb" "get each wrench")

  (run-command-print! "caleb@example.com caleb" "drink glass")

  (run-command-print! "caleb@example.com caleb" "get glass")

  (run-command-print! "caleb@example.com caleb" "west")


  )
