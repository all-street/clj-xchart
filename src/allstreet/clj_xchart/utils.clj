(ns allstreet.clj-xchart.utils)

;; reduce-map + map-vals is taken from the Medley utility library:
;; https://github.com/weavejester/medley
;; Medley is under the same license (EPL1.0) as clj-xchart.
(defn- reduce-map [f coll]
  (if (instance? clojure.lang.IEditableCollection coll)
    (persistent! (reduce-kv (f assoc!) (transient (empty coll)) coll))
    (reduce-kv (f assoc) (empty coll) coll)))

(defn map-vals
  "Maps a function over the values of an associative collection."
  [f coll]
  (reduce-map (fn [xf] (fn [m k v] (xf m k (f v)))) coll))

(defn- transpose-single
  [acc k1 v1]
  (reduce-kv (fn [m k2 v2]
               (assoc-in m [k2 k1] v2))
             acc v1))

(defn transpose-map
  "Transforms a map of maps such that the inner keys and outer keys are flipped.
  That is, `(get-in m [k1 k2])` = `(get-in (transpose-map m) [k2 k1])`. The
  inner values remain the same."
  [series]
  (reduce-kv transpose-single {} series))

(defn extract-series
  "Transforms coll into a series map by using the values in the provided keymap.
  There's no requirement to provide :x or :y (or any key at all, for that
  matter), although that's common.

  Example: (extract-series {:x f, :y g, :bubble bubble} coll)
        == {:x (map f coll), :y (map g coll), :bubble (map bubble coll)}"
  [keymap coll]
  (map-vals #(map % coll) keymap))

(defn- normalize-group
  [m]
  (let [sum (reduce + (vals m))]
    (map-vals #(/ % sum) m)))

(defn normalize-categories
  [m]
  (->> (transpose-map m)
       (map-vals normalize-group)
       transpose-map))
