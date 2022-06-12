(ns ionsails.util.trie)

(defn- add-to-trie [trie x]
  (assoc-in trie x (merge (get-in trie x) {:terminal true})))

(defn- flatten-map-keys
  ([form]
   (flatten-map-keys form nil))
  ([form pre]
   (mapcat (fn [[k v]]
             (let [prefix (if pre (str pre (name k)) (name k))]
               (if (map? v)
                 (flatten-map-keys v prefix)
                 [pre])))
           form)))

(defn in-trie? [trie x]
  "Returns true if the value x exists in the specified trie."
  (contains? (get-in trie (vec x)) :terminal))

(defn lookup [trie prefix]
  "Returns a list of matches with the prefix specified in the trie specified."
  (let [sub-trie (get-in trie (vec prefix))]
    (map #(str prefix %) (flatten-map-keys sub-trie))))

(defn build-trie [coll]
  "Builds a trie over the values in the specified seq coll."
  (reduce add-to-trie {} coll))
