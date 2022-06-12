(ns ionsails.core.parse
  (:require [clojure.string :as str]
            [instaparse.failure :as inf]
            #?(:cljs  [instaparse.core :refer-macros [defparser]]
               :clj  [instaparse.core :refer [defparser]])))

(defparser command-parser "
  COMMAND    = COM_NAME ARG*
  COM_NAME   = #'^\\w+'
  ARG        = (REF | QUANT_REF | PREFIX_REF | SUFFIX_REF | RES_WORD)
  SUFFIX_REF = REF SUFFIX
  PREFIX_REF = PREFIX REF
  QUANT_REF  = NUM REF
  REF        = (EID | RANK | QUOTE | STR)
  RES_WORD = 'all'
  PREFIX   = 'each' | 'at' | 'in' | 'from' | 'to' | 'into'
  SUFFIX   = 'out'
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

(defn- process-parser-data-ref
  [[tag-name value]]
  (condp = tag-name
    :STR {:keywords #{value}}
    :RANK {:rank (->int (subs value 1))}
    :QUOTE {:keywords (->unquoted value)}
    :EID {:eid (->int (subs value 1))}
    :else {}))

(defn- process-parser-data-quant-ref
  [form]
  (let [[num-pair ref-pair] form
        [_ num-value] num-pair
        [_ ref-value] ref-pair
        quantity (->int num-value)
        resolved-ref-map (process-parser-data-ref ref-value)]
    (assoc resolved-ref-map :quantity quantity)))

(defn- process-parser-data-prefix-ref
  [form]
  (let [[[_ prefix-value] [_ ref-value]] form
        prefix (keyword prefix-value)
        resolved-ref-map (process-parser-data-ref ref-value)]
    (assoc resolved-ref-map :modifier prefix)))

(defn- process-parser-data-suffix-ref
  [form]
  (let [[[_ ref-value] [_ suffix-value]] form
        suffix (keyword suffix-value)
        resolved-ref-map (process-parser-data-ref ref-value)]
    (assoc resolved-ref-map :modifier suffix)))

(defn- process-parser-arg-data
  "Processes each form that can be in ARG"
  [arg]
  (let [[tag-name & data] arg
        result (condp = tag-name
                 :REF (process-parser-data-ref (first data))
                 :QUANT_REF (process-parser-data-quant-ref data)
                 :PREFIX_REF (process-parser-data-prefix-ref data)
                 :SUFFIX_REF (process-parser-data-suffix-ref data)
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
