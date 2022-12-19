(ns ionsails.core.commands
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [ionsails.core.defcommand :as dc :refer [defcommand defalias *db* *sender* *args*]]
            [ionsails.core.messages :refer [item info dm-info dm-warn dm-err dm] :as m]
            [ionsails.core.titles  :as ti]
            [ionsails.core.queries :as q]
            [ionsails.core.effects :as e]
            [ionsails.core.systems.flammable :as flam]
            [ionsails.core.systems.quantifiable :as quant])
  #?(:cljs (:require-macros [clojure.core.match :refer [match]]
                            [ionsails.core.defcommand :refer [defcommand defalias]])))

;; CORE/BUILTINS

(defcommand help
  "Use help to see information for a given command or topic. See `help commands` for a list of commands. (Messages given inside of `'s indicate what to type in the box below)"
  (let [num-args (count *args*)
        topic (-> *args* first :keywords first)
        command-doc (dc/get-doc topic)]
    (match [num-args topic (some? command-doc)]
           [0 _ _] (dm-info (dc/get-doc "help"))
           [1 "commands" _] (dm-info "Available commands are: " (dc/cmd-list))
           [1 _ true] (dm-info (dc/get-doc topic))
           [1 _ false] (dm-info "No help on that topic")
           :else (dm-err "Incorrect syntax - see `help`"))))

(defcommand handle-failure'
  "Handles command failure"
  (dm-err (:message (first *args*))))

(defcommand missing-command'
  "Handles command does not exist error"
  (when-let [msg (:message (first *args*))]
    (dm-err (str "Error - The command: \"" msg "\" does not exist. See `help commands`"))))

;; data utilities

(defn is-unique-name-query? [kws-or-name]
  (and (set? kws-or-name)
       (= (count kws-or-name) 1)
       (str/includes? (first kws-or-name) "-")))

;; ITEMS


(defn put-item-in [item-kws container-kws]
  (let [item (q/find-in-inventory-query *db* *sender* item-kws)
        container (or (q/find-in-inventory-query *db* *sender* container-kws)
                      (q/find-in-room-query *db* *sender* container-kws))
        player (q/player-query *db* *sender*)
        item-details (when item (q/item-details *db* item))
        item-title (ti/get-title item-details)
        container-details (when container (q/item-details *db* container))
        container-title (ti/get-title container-details)]
    (if (and player container item)
      (let [effects {:effects (e/move-contents-no-merge item player container)}
            reply-msg (dm-info "You put " item-title " into " container-title)]
        (merge effects reply-msg))
      (dm-warn "You see nothing like that here."))))

(defcommand put
  "put - "
  (let [num-args (count *args*)
        [arg1 arg2] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier (:modifier arg2)]
      [2 _ _] (put-item-in keywords (:keywords arg2))
      :else (dm-err "Incorrect syntax - see `help put`"))))

(defn get-item-in [item-kws container-kws]
  (let [ent-map (q/find-in-container-query *db* *sender* container-kws item-kws)
        {:keys [player container item]} ent-map
        item-details (when item (q/item-details *db* item))
        item-title (ti/get-title item-details)
        can-hold-item? (:can-hold? item-details)
        container-details (when container (q/item-details *db* container))
        container-title (ti/get-title container-details)]
    (if (and player container item can-hold-item?)
      (let [effects {;; FIXME: replace with mergable version for materials
                     :effects (e/move-contents-no-merge item container player)}
            reply-msg (dm-info "You get " item-title " from " container-title)]
        (merge effects reply-msg))
      (if (and container item (not can-hold-item?))
        (dm-warn "You cannot remove that from it's container")
        (if container
          (dm-warn "You see nothing like that in " container-title)
          (dm-warn "You see nothing like that here."))))))

(defn get-all
  [kw-in]
  (let [ent-maps (q/find-player-room-items-in-room-query *db* *sender* kw-in)
        {:keys [room player]} (first ent-maps)
        items-details (q/items-details *db* (map :item ent-maps))
        items-details (filter :can-hold? items-details)]
    (if (seq items-details)
      (let [{:keys [effects message-body]}
            (reduce (fn [agg item-detail]
                      (let [effect (e/move-contents-no-merge (:db/id item-detail) room player)
                            item-title (ti/get-title item-detail)
                            msg (info "You pick up " item-title)]
                        (-> agg
                            (update :effects concat effect)
                            (update :message-body conj msg))))
                    {:effects []
                     :message-body []}
                    items-details)]
        (merge {:effects effects} (dm message-body)))
      (dm-warn "You cannot pick anything up here."))))

(defn get-item [kws]
  (let [ent-map (q/find-player-room-item-in-room-query *db* *sender* kws)
        {:keys [player room item]} ent-map
        item-details (when item (q/item-details *db* item))
        item-holdable (:can-hold? item-details)]
    (if (and player room item item-holdable)
      (let [effects {:effects (e/move-contents-no-merge item room player)}
            item-title (ti/get-title item-details)
            reply-msg (dm-info "You pick up " item-title)]
        (merge effects reply-msg))
      (if item
        (dm-warn "You cannot pick that up.")
        (dm-warn "You see nothing like that here.")))))

;; FIXME: add quantity
(defcommand get
  "get - "
  (let [num-args (count *args*)
        [arg1 arg2] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier (:modifier arg2)]
      [0 _ _] (dm-err "Incorrect syntax for get command - see `help get`")
      [1 :all _] (get-all keywords)
      [1 :each _] (get-all keywords)
      [1 _ _] (get-item keywords)
      [2 _ :in] (get-item-in keywords (:keywords arg2))
      [2 _ :from] (get-item-in keywords (:keywords arg2))
      :else (dm-err "Incorrect syntax - see `help look`"))))

(defcommand drop
  "drop - "
  (let [kws (-> *args* first :keywords)
        ent-map (q/find-player-room-item-in-inv-query *db* *sender* kws)
        {:keys [player room item]} ent-map
        item-details (when item (q/item-details *db* item))]
    (if (and player room item)
      (let [effects {:effects (e/move-contents-no-merge item player room)}
            item-title (ti/get-title item-details)
            reply-msg (dm-info "You drop " item-title)]
        (merge effects reply-msg))
      (dm-warn "You have nothing like that to drop."))))

(defcommand inventory
  "inventory - Shows the items you are holding"
  (let [inv (q/player-inventory *db* *sender*)
        titles (map ti/get-title inv)
        title-msgs (mapv item titles)
        msgs (if (empty? title-msgs)
               [(info "Your inventory is empty")]
               (concat [(info "Your inventory includes:")] title-msgs))]
    (dm msgs)))

(defalias inv 'inventory)
(defalias i 'inventory)

;; LOOK

(defn format-room-data [sender look-room-res]
  (let [{:keys [description exits contents]} look-room-res
        exit-names (->> exits (remove #(= (:closed? %) true)) (map :direction) (map name))
        room-content-outf (fn [v]
                            (let [{:keys [can-hold? can-free-will? can-transport? owner]} v
                                  outm (info (ti/get-title v))]
                              (cond
                                (= owner sender) nil
                                can-hold? (assoc outm :category :item)
                                can-free-will? (assoc outm :category :mob)
                                can-transport? (assoc outm :category :vehicle)
                                :else outm)))
        content-output (->> contents (mapv room-content-outf))]
    (concat
     [{:category :title :text (ti/get-title look-room-res)}
      {:category :info :text description}
      {:category :exit :text (apply str "Exits: " exit-names)}]
     content-output)))

(defn look-room [] (dm (format-room-data *sender* (q/look-room *db* *sender*))))

(defn format-ent-detail-data
  [ent]
  (let [{:keys [can-hold? description health]} ent
        headline-category (if can-hold? :item :title)]
    (concat
     [{:category headline-category :text (str "You see " description ".")}]
    ;; FIXME: Create health description lookup
     (if (= health 100)
       [{:category :info :text "It is in perfect health."}]
       [{:category :info :text "It is damaged."}]))))

(defn format-ent-highlight-data
  [ent]
  {:category :info :text (ti/get-title ent)})

(defn look-at [keywords]
  (let [ent (q/look-at *db* *sender* keywords)]
    (if ent
      (dm (ti/get-description ent))
      (dm-warn "You see nothing like that here."))))

(defn look-in [keywords]
  (if-let [ent (q/query-in-container *db* *sender* keywords)]
    (dm (ti/get-description ent))
    (dm-warn "You see nothing like that here")))

(defn look-at-in
  [container-kws inner-kws]
  (if-let [ent (q/query-at-item-in-container *db* *sender* container-kws inner-kws)]
    (dm (ti/get-description ent))
    (dm-warn "You see nothing like that here.")))

(defcommand look
  "look command help text goes here..."
  (let [num-args (count *args*)
        [arg1 arg2] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier (:modifier arg2)]
           [0 _ _] (look-room)
           [1 :in _] (look-in keywords)
           [1 _ _] (look-at keywords)
           [2 _ :in] (look-at-in (:keywords arg2) keywords)
           :else (dm-err "Incorrect syntax - see `help look`"))))

(defalias l 'look)

;; MOVEMENT

(defn player-move [direction]
  (if-let [{:keys [source target entity]:as move-args} (q/move-room-eids *db* *sender* direction)]
    (let [res {:effects (e/move-contents-no-merge entity source target)
               :force-command "look"}
          messages (dm-info "You walk to the " (name direction))]
      (prn res)
      (merge res messages))
    (dm-err "There is no open exit in that direction")))

(defcommand west
  "west - Moves the player west if possible"
  (player-move :west))

(defcommand east
  "east - Moves the player east if possible"
  (player-move :east))

(defcommand north
  "north - Moves the player north if possible"
  (player-move :north))

(defcommand south
  "south - Moves the player south if possible"
  (player-move :south))

(defcommand up
  "up - Moves the player vertically up if possible"
  (player-move :up))

(defcommand down
  "down - Moves the player vertically down if possible"
  (player-move :down))

(defalias n 'north)
(defalias s 'south)
(defalias e 'east)
(defalias w 'west)
(defalias u 'up)
(defalias d 'down)

;; fire

(defn handle-light
  [keywords]
  (if-let [item (q/find-in-inventory-or-room-query *db* *sender* keywords)]
    (let [item-details (q/vessel-details *db* item)
          item-title (ti/get-title item-details)]
      ;; TODO: Add requirements to light some items using tools or lit items
      (if (flam/can-light? item-details)
        (if-not (flam/burning? item-details)
          (let [flam-targets (if (:burn-rate item-details)
                               [item-details]
                               (flam/get-flammables item-details))
                {:keys [burn-rate consumed-amounts]} (flam/find-burned-amounts flam-targets)
                consume-effects (e/consume-by-template-mapping flam-targets consumed-amounts)
                burn-effects (flam/set-burn *db* item burn-rate)
                effects (concat consume-effects burn-effects)]
            (assoc
             (dm-info "You light " item-title)
             :effects effects))
          (dm-err item-title " is alread lit."))
        (dm-err item-title " cannot be lit on fire.")))
    (dm-err "There is nothing like that to light.")))

(defn handle-extinguish
  [keywords]
  (if-let [item (q/find-in-inventory-or-room-query *db* *sender* keywords)]
    (let [item-details (q/vessel-details *db* item)
          item-title (ti/get-title item-details)]
      (if (flam/can-light? item-details)
        (if (flam/burning? item-details)
          (let [effects (flam/set-extinguish *db* item-details)]
            (assoc (dm-info "You put out " item-title)
                   :effects effects))
          (dm-err item-title " is not currently burning."))
        (dm-err item-title " cannot be lit on fire.")))
    (dm-err "There is nothing like that here.")))

(defcommand extinguish
  ""
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier]
      [0 _] (dm-err "You must specify what you wish to extinguish - see `help extinguish`")
      [1 _] (handle-extinguish keywords)
      :else (dm-err "Incorrect syntax - see `help extinguish`"))))

(defcommand light
  ""
(let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier]
      [0 _] (dm-err "You must specify what you wish to light - see `help light`")
      [1 _] (handle-light keywords)
      :else (dm-err "Incorrect syntax - see `help light`"))))

;; TODO: defcommand extinguish


;; liquids


(defn handle-drink
  [keywords]
  (let [{:keys [player container]} (q/find-in-vessel-query *db* *sender* keywords)
        container-details (when container (q/vessel-details *db* container))
        liquids (quant/get-liquids container-details)
        desired-consumption (or (:rate container-details) 50)
        container-title (ti/get-title container-details)]
    (if (empty? liquids)
      (dm-warn "There's nothing there to drink")
      (let [consumed-amounts-map (quant/find-consumed-amounts desired-consumption liquids)
            effects (e/consume-by-template-mapping liquids consumed-amounts-map)]
        (if container
          (assoc (dm-info "You drink from " container-title)
                 :effects effects)
          (dm-warn "You're not holding anything like that or containing that drink."))))))

;; TODO: add empty of any solids
(defn handle-empty
  [src-kws & [pour-amount-limit]]
  (if-let [{:keys [room item]} (q/find-player-room-item-in-inv-query *db* *sender* src-kws)]
    (let [source-details (q/vessel-details *db* item)
          target-details (q/vessel-details *db* room)
          source-liquids (quant/get-liquids source-details)
          is-source-vessel? (:holds-liquid? source-details)
          source-title (ti/get-title source-details)
          consumed-amounts-map (if (some? pour-amount-limit)
                                 (quant/find-consumed-amounts pour-amount-limit source-liquids)
                                 (quant/find-consume-all-map source-liquids))]
      (if (and is-source-vessel? (seq consumed-amounts-map))
        (let [
              src-effects (e/consume-by-template-mapping source-liquids consumed-amounts-map)
              target-effects (e/create-by-template-mapping source-liquids target-details consumed-amounts-map)
              effects (concat src-effects target-effects)
              msg (if (some? pour-amount-limit)
                    (str "You empty some of " source-title " onto the ground.")
                    (str "You empty " source-title " onto the ground."))]
          (assoc
           (dm-info msg)
           :effects effects))
        (dm-warn source-title " is already empty.")))
    (dm-warn "Cannot find anything like that to empty.")))

(defn handle-pour
  [target-kws source-kws & [pour-amount-limit]]
  (let [{source :item player :player} (q/find-vessel-query *db* *sender* source-kws)
        {target :item} (q/find-vessel-query *db* *sender* target-kws)
        target-details (when target (q/vessel-details *db* target))
        source-details (when source (q/vessel-details *db* source))
        is-target-vessel? (:holds-liquid? target-details)
        is-source-vessel? (:holds-liquid? source-details)
        target-title (ti/get-title target-details)
        source-title (ti/get-title source-details)
        [target-liquids target-capacity] ((juxt quant/get-liquids :capacity) target-details)
        source-liquids (quant/get-liquids source-details)
        target-liquid-amounts (map :quantity target-liquids)
        source-liquid-total (apply + (map :quantity source-liquids))
        target-liquid-total (apply + target-liquid-amounts)
        remaining-in-target (- target-capacity target-liquid-total)]
    (if (and player source target is-target-vessel? is-source-vessel?)
      (let [goal-amount (if (some? pour-amount-limit)
                          (min remaining-in-target pour-amount-limit)
                          remaining-in-target)
            consumed-amounts-map (quant/find-consumed-amounts
                                  goal-amount source-liquids)
            src-effects (e/consume-by-template-mapping source-liquids consumed-amounts-map)
            target-effects (e/create-by-template-mapping source-liquids target-details consumed-amounts-map)
            effects (concat src-effects target-effects)
            liquids-str (ti/get-liquids-title source-liquids)]
        (if (zero? source-liquid-total)
          (dm-warn source-title " does not have anything in it.")
          (if (zero? remaining-in-target)
            (dm-warn target-title " cannot hold any more.")
            (assoc (dm-info "You fill " (:title target-details) " with " liquids-str " from " (:title source-details))
                   :effects effects))))
      (if (and source target)
        (if is-target-vessel?
          (dm-warn source-title " does not have anything to pour.")
          (dm-warn target-title " cannot hold liquids."))
        (if source
          (dm-warn "You see nothing like that to pour liquids into.")
          (dm-warn "You see nothing like that here as a source of liquids."))))))

(defcommand fill
  "Fills a target fluid container from a source, usually another fluid container.
   Takes as much as the target can fit. Use `pour` to specify a quantity to pour"
  (let [num-args (count *args*)
        [arg1 arg2] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier]
           [0 _] (dm-err "You must specify what you wish to fill and from what - see `help fill`")
           [1 _] (dm-err "You must specify what you wish to fill and from what - see `help fill`")
           [2 _] (handle-pour keywords (:keywords arg2))
           :else (dm-err "Incorrect syntax - see `help fill`"))))

(defcommand pour
  "pour liquids"
  (let [num-args (count *args*)
        [arg1 arg2] *args*
        {:keys [keywords modifier quantity]} arg1
        target (:keywords arg2)]
    (match [num-args modifier (some? quantity)]
      [0 _ _] (dm-err "You must specify how much to pour, what from, and what to pour into - see `help pour`")
      [1 _ _] (dm-err "You must specify how much to pour, what from, and what to pour into - see `help pour`")
      [2 _ false] (dm-err "You must specify how much to pour - see `help pour`")
      [2 _ true]  (handle-pour target keywords quantity)
      :else (dm-err "Incorrect syntax - see `help pour`"))))

(defcommand empty
  "Empties a container of all contents onto the ground, including any fluids. Mostly used to pour out unwated liquids"
  (let [num-args (count *args*)
        [arg1 arg2] *args*
        {:keys [keywords quantity]} arg1]
    (match [num-args ]
      [0] (dm-err "You must specify what container to empty - see `help empty`")
      [1] (handle-empty keywords quantity)
      :else (dm-err "Incorrect syntax - see `help pour`"))))

(defcommand drink
  "Drink a liquid from a container you are holding. Specify the container or a liquid within it."
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier]
      [0 _] (dm-err "You must specify what you wish to drink - see `help drink`")
      [1 _] (handle-drink keywords)
      [1 :from] (handle-drink keywords)
      :else (dm-err "Incorrect syntax - see `help drink`"))))

;; Equipment

(defn unequip-msg
  [item-details]
  (let [eq-loc (:equip-location item-details)
        item-title (ti/get-title item-details)
        [reply-prefix reply-suffix]
(if (#{:hand :hands} eq-loc)
                                          ["You stop holding " (str " in your " (name eq-loc))]
                                          ["You stop wearing " (str " on your " (name eq-loc))])
        ]
    (str reply-prefix item-title reply-suffix)))

(defn handle-unequip
  [kws]
  (let [{:keys [player room item]} (q/find-player-room-item-in-equips-query *db* *sender* kws)
        item-details (when item (q/item-details *db* item))]
    (if (and player room item)
      (let [effects {:effects (e/unequip item player player)}
            reply-msg (dm-info (unequip-msg item-details))]
        (merge effects reply-msg))
      (dm-warn "You have nothing like that equipped."))))

(defn handle-unequip-all
  []
  (let [ent-maps (q/find-player-room-items-in-equips-query *db* *sender* nil)
        {:keys [player]} (first ent-maps)
        items-details (q/items-details *db* (map :item ent-maps))
        items-details (filter :equip-location items-details)]
    (if (seq items-details)
      (let [{:keys [effects message-body]}
            (reduce (fn [agg item-detail]
                      (let [effect (e/unequip (:db/id item-detail) player player)
                            msg {:category :info :text (unequip-msg item-detail)}]
                        (-> agg
                            (update :effects concat effect)
                            (update :message-body conj msg))))
                    {:effects []
                     :message-body []}
                    items-details)]
        (merge {:effects effects} (dm message-body)))
      (dm-info "You have nothing equipped."))))

(defn equip-msg
  [item-details]
  (let [eq-loc (:equip-location item-details)
        item-title (ti/get-title item-details)
        [reply-prefix reply-suffix] (if (#{:hand :hands} eq-loc)
                                      ["You hold " (str " in your " (name eq-loc))]
                                      ["You wear " (str " on your " (name eq-loc))])]
    (str reply-prefix item-title reply-suffix)))

(defn handle-equip
  [kws]
  (let [{:keys [player room item]} (q/find-player-room-item-in-inv-query *db* *sender* kws)
        item-details (when item (q/item-details *db* item))
        item-equipable? (some? (:equip-location item-details))]
    (if (and player room item item-equipable?)
      (let [effects {:effects (e/equip item player player)}
            reply-msg (dm-info (equip-msg item-details))]
        (merge effects reply-msg))
      (if item
        (dm-warn "That is not something that can be worn, wielded or otherwise equipped by you.")
        (dm-warn "You have nothing like that to equip.")))))

(defn handle-equip-all
  []
  (let [ent-maps (q/find-player-room-items-in-inv-query *db* *sender* nil)
        {:keys [player]} (first ent-maps)
        items-details (q/items-details *db* (map :item ent-maps))
        items-details (filter :equip-location items-details)]
    (if (seq items-details)
      (let [{:keys [effects message-body]}
            (reduce (fn [agg item-detail]
                      (let [effect (e/equip (:db/id item-detail) player player)
                            msg {:category :info :text (equip-msg item-detail)}]
                        (-> agg
                            (update :effects concat effect)
                            (update :message-body conj msg))))
                    {:effects []
                     :message-body []}
                    items-details)]
        (merge {:effects effects} (dm message-body)))
      (dm-warn "You have nothing in your inventory to equip."))))

(defcommand equipment
  "equipment - Shows the items you are holding or wearing"
  (let [eq (q/player-equipment *db* *sender*)
        titles (map ti/get-title eq)
        title-msgs (mapv (fn [title] {:category :item :text title}) titles)
        msgs (if (empty? title-msgs)
               [{:category :info :text  "You have nothing equipped"}]
               (concat [{:category :info :text  "You have equipped:"}] title-msgs))]
    (dm msgs)))

(defcommand unequip
  "Stop using a piece of equipment such as clothing or a weapon.
Also used to deactivate certain items being used together."
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords modifier]} arg1]
    (match [num-args modifier]
      [0 _] (dm-err "You must specify what you wish to unequip - see `help unequip`")
      [1 nil] (handle-unequip keywords)
      [1 :all] (handle-unequip-all)
      :else (dm-err "Incorrect syntax - see `help unequip`"))))

(defcommand equip
  "Begin using a piece of equipment such as clothing or a weapon.
  Also used to activate or use certain items in conjunction with each other."
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords modifier]} arg1]
    (prn *args*)
    (match [num-args modifier]
      [0 _] (dm-err "You must specify what you wish to equip - see `help equip`")
      [1 nil] (handle-equip keywords)
      [1 :all] (handle-equip-all)
      :else (dm-err "Incorrect syntax - see `help equip`"))))



(defalias wear 'equip)
(defalias eq 'equipment)
(defalias remove 'unequip)

;; ADMIN commands

(defn handle-inspect
  [query-term]
  (if-let [typ (cond
                 (number? query-term) :eid
                 (is-unique-name-query? query-term) :template-name
                 :else :keywords)]
    (if-let [lookup (condp = typ
                      :eid query-term
                      :template-name [:template-name (first query-term)]
                      :keywords (q/find-in-inventory-or-room-query *db* *sender* query-term))]
      (if-let [val (q/find-by-eid *db* *sender* lookup)]
        (let [msg (->> val
                       ti/get-edn
                       str/split-lines
                       (mapv (fn [t] {:category :edn :text t})))]
          (dm msg))
        (dm-warn "Nothing found matching that query."))
      (dm-warn "Nothing found matching those keywords."))
    (dm-err "Invalid argument. Must be EID, template-name or keywords")))

(defn handle-query
  [kws]
  (if-let [templates (q/template-search *db* *sender* kws)]
    (let [edn-lines (->> templates
                         (map ti/get-title)
                         (map str/split-lines)
                         (apply concat))
          msg (mapv (fn [t] {:category :edn :text t})
                    (concat ["["] edn-lines ["]"]))]
      (dm msg))
    (dm-warn "Nothing found for that keyword query")))


(defn handle-instantiation
  "helper for spawn and clone commands. arg must be an EID or template-name"
  [arg]
  (if-let [lookup (cond
                    (number? arg) arg
                    (is-unique-name-query? arg) [:template-name (first arg)])]
    (let [room (q/look-room-query *db* *sender*)
          room-details (q/vessel-details *db* room)
          ent (q/find-by-eid *db* *sender* lookup)
          new-ent (dissoc ent :template-name)]
      (if (and room ent)
        (assoc
         (dm-info "You spawn " (ti/get-title new-ent))
         :effects [(e/create-within new-ent room-details)])
        (dm-warn "There is not an entity that matches that unique entity lookup.")))
    (dm-err "That query is not valid as a unique entity lookup")))

(defn handle-spawn
  [kws-or-name]
  (if (is-unique-name-query? kws-or-name)
    (handle-instantiation kws-or-name)
    (dm-err "Not a valid exact match query. Must be a unique name")))

(defn handle-clone
  [eid-num]
  (handle-instantiation eid-num))

(defcommand spawn
  "Spawn a new entity based on the given unique template name"
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords]} arg1]
    (match [num-args]
      [0] (dm-err "You must specify what to spawn - see `help spawn`")
      [1] (handle-spawn keywords)
      :else (dm-err "Incorrect syntax - see `help spawn`"))))

(defcommand clone
  "Create a copy of a given entity given by eid. Cannot be used"
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [eid]} arg1]
    (match [num-args (some? eid)]
      [0 _] (dm-err "You must specify what to clone by EID such as `clone @1`- see `help clone`")
      [1 false] (dm-err "Clone requires an EID, which is a number preceded by @ indicating a unique entity - see `help clone`")
      [1 true] (handle-clone eid)
      :else (dm-err "Incorrect syntax - see `help spawn`"))))

(defcommand inspect
  ""
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords eid]} arg1]
    (match [num-args (some? eid) (some? keywords)]
      [0 _ _] (dm-err "You must specify what to inspect by EID such as `inspect @1` or keyords - see `help inspect`")
      [1 false false] (dm-err "inspect requires an EID, or keywords - see `help inspect`")
      [1 true false] (handle-inspect eid)
      [1 false true] (handle-inspect keywords)
      :else (dm-err "Incorrect syntax - see `help spawn`"))))

(defcommand query
  "Query for an entity template using a unique name or keywords"
  (let [num-args (count *args*)
        [arg1] *args*
        {:keys [keywords]} arg1]
    (match [num-args (is-unique-name-query? keywords)]
      [0 _] (dm-err "You must specify what to query for - see `help query`")
      [1 false] (handle-query keywords)
      [1 true] (handle-inspect keywords)
      :else (dm-err "Incorrect syntax - see `help query`"))))
