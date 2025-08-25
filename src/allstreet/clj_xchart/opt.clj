(ns allstreet.clj-xchart.opt
  "A namespace for clj-xchart with several optimisations when you run on big
  datasets.")

(defn extract-series
  "Transforms coll into a series map by using the values in the
  provided keymap with exctract-field. There's no requirement to
  provide :x or :y (or any key at all, for that matter), although
  that's common.

  Example: (extract-series {:x f, :y g, :bubble bubble} coll)
        == {:x (extract-field f coll),
            :y (extract-field g coll),
            :bubble (extract-field bubble coll)}"
  [keymap coll]
  (into {}
        (for [[k v] keymap]
          [k (eduction (map v) coll)])))
