(ns allstreet.clj-xchart.fonts
  (:import [java.awt Font]))

;; set-legend! -> set-default-style!
;; set-chart-title-style! -> set-chart-style! -> set-default-style!
;; set-axis-ticks! -> set-axes-style!
;; set-axis-title! -> set-axes-style!

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
