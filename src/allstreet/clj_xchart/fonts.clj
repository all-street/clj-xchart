(ns allstreet.clj-xchart.fonts
  (:import [java.awt Font]))

(def font-families
  {:dialog Font/DIALOG
   :dialog-input Font/DIALOG_INPUT
   :monospaced Font/MONOSPACED
   :sans-serif Font/SANS_SERIF
   :serif Font/SERIF})

(def font-styles
  {:bold Font/BOLD
   :italic Font/ITALIC
   :plain Font/PLAIN})

(defn make-font
  [{:keys [name style size] :or
    {name :dialog style :plain size 13}}]
  (Font. (font-families name name)
         (font-styles style style)
         size))

(defn ->font [font]
  (cond (instance? Font font) font
        (map? font) (make-font font)
        ;; TODO: parse String font specs
        (string? font) (make-font {})))

(defn- assoc-in-nonexisting
  "Like assoc-in, but will only add fields not found. Nil may be found, in which
  case it is NOT updated."
  [m ks v]
  (cond-> m
    (identical? (get-in m ks ::not-found) ::not-found)
    (assoc-in ks v)))

(defn attach-default-font
  "Sets the font type on all the provided "
  [{:keys [font] :as style-map}]
  (if-let [font (->font font)]
    (-> style-map
        (dissoc style-map :font)
        (assoc-in-nonexisting [:axis :ticks :labels :font] font)
        (assoc-in-nonexisting [:axis :title :font] font)
        (assoc-in-nonexisting [:legend :font] font)
        (assoc-in-nonexisting [:annotations-font] font)
        (assoc-in-nonexisting [:chart :title :font] font))
    style-map))
