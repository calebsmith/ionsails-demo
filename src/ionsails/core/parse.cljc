(ns ionsails.core.parse
  (:require [clojure.string :as str]
            [instaparse.failure :as inf]
            #?(:cljs  [instaparse.core :refer-macros [defparser]]
               :clj  [instaparse.core :refer [defparser]])))

(defparser command-parser "
  COMMAND    = COM_NAME ARG*
  COM_NAME   = #'^\\w+'
  ARG        = (REF | PRE_REF | REF_RANK | PRE_REF_RANK | RES_WORD)
  PRE_REF_RANK = (NUM | PREFIX) REF RANK
  REF_RANK   = REF RANK
  PRE_REF    = (NUM | PREFIX) REF
  REF        = (QUOTE | STR | EID)
  RES_WORD = 'all'
  PREFIX   = 'each' | 'at' | 'in' | 'from' | 'to' | 'into'
  RANK     = #'#\\d+'
  EID      = #'@\\d+'
  QUOTE    = #'\"[a-zA-Z -]+\"'
  STR      = #'[a-zA-Z-]+'
  NUM      = #'\\d+'
" :auto-whitespace :standard)

#?(:cljs (defn- ->int
           [n]
           (js/parseInt n))
   :clj (defn- ->int
             [n]
             (Integer/parseInt n)))

(defn- ->unquoted
  "Given a string surrounded by quotes, return the list of space separated words without the surrounding quotes"
  [v]
  (set
   (str/split
    (subs v 1 (dec (count v))) #" ")))

(defn- ->stripped-int
  [rank-or-eid]
  (->int (subs rank-or-eid 1)))

(defn- process-parser-data-ref
  [[tag-name value]]
  (condp = tag-name
    :STR {:keywords #{value}
          :rank 1}
    :QUOTE {:keywords (->unquoted value)
            :rank 1}
    :EID {:eid (->stripped-int value)}
    :else {}))

(defn- process-parser-data-pre-ref-rank
  [form]
  (let [[[pre-tag pre-raw-v] [_ ref-value] [_ rank-value]] form
        [attr pre-v] (if (= pre-tag :PREFIX)
                       [:modifier (keyword pre-raw-v)]
                       [:quantity (->int pre-raw-v)])
        resolved-ref-map (process-parser-data-ref ref-value)
        rank (->stripped-int rank-value)]
    (assoc resolved-ref-map attr pre-v :rank rank)))

(defn- process-parser-data-ref-rank
  [form]
  (let [[ref-pair rank-pair ] form
        [_ ref-value] ref-pair
        [_ rank-value] rank-pair
        rank (->stripped-int rank-value)
        resolved-ref-map (process-parser-data-ref ref-value)]
    (assoc resolved-ref-map :rank rank)))


(defn- process-parser-data-pre-ref
  [form]
  (let [[[pre-tag pre-raw-v] [_ ref-value]] form
        [attr pre-v] (if (= pre-tag :PREFIX)
                       [:modifier (keyword pre-raw-v)]
                       [:quantity (->int pre-raw-v)])
        resolved-ref-map (process-parser-data-ref ref-value)]
    (assoc resolved-ref-map attr pre-v)))

(defn- process-parser-arg-data
  "Processes each form that can be in ARG"
  [arg]
  (let [[tag-name & data] arg
        result (condp = tag-name
                 :REF (process-parser-data-ref (first data))
                 :PRE_REF (process-parser-data-pre-ref data)
                 :PRE_REF_RANK (process-parser-data-pre-ref-rank data)
                 :REF_RANK (process-parser-data-ref-rank data)
                 :RES_WORD {:modifier (keyword (first data))}
                 :else (fn [_] {}))]
    result))

(defn- normalize-parse-results
  "Given parse results from command-parser, normalizes the parse tree into a two-tuple with the command name and a vector of maps describing the command arguments if any"
  [p-res]
  (let [;; Separate out the command name and its arguments tree
        [_ [_ cmd] & raw-args] p-res
        args (->> raw-args
                  ;; Transforms [:ARG x] -> x
                  (mapv (fn [[_ data]] data))
                  ;; Process each node in the top form into a flat hash-map
                  (mapv process-parser-arg-data))]
    [cmd args]))

(defn format-parse-failure
  [failure]
  (let [{:keys [text column]} failure
        i (dec column)
        marker-txt (str (subs text 0 i) "^" (subs text i))]
    (str "Bad command arguments: " marker-txt ". Expected more at ^")))

(defn parse-command
  "Parses a command string into structured data for command handlers"
  [v]
  (when (seq v)
    (let [parse-results (command-parser v)]
      (if (vector? parse-results)
        (normalize-parse-results parse-results)
        ["handle-failure'" [{:message (format-parse-failure parse-results)}]]))))
