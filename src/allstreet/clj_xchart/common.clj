(ns allstreet.clj-xchart.common
  (:require
   [allstreet.clj-xchart.fonts :as fonts])
  (:import [org.knowm.xchart.style.theme
            GGPlot2Theme MatlabTheme XChartTheme]))



(def themes
  "The different default themes you can use with xchart."
  {:ggplot2 (GGPlot2Theme.)
   :matlab (MatlabTheme.)
   :xchart (XChartTheme.)})

(defmacro doto-cond
  "Example:
  (doto-cond expr
    cond1 (my call)
    cond2 (my2 call2))
  =>
  (let [e# expr]
    (when cond1 (my #e call))
    (when cond2 (my2 #2 call2)))"
  [expr & clauses]
  (let [pairs (partition 2 clauses)
        expr-sym (gensym "expr")]
    `(let [~expr-sym ~expr]
       ~@(map (fn [[cond clause]]
                `(when ~cond
                   (~(first clause) ~expr-sym ~@(rest clause))))
              pairs)
       ~expr-sym)))

(defn add-raw-series
  ([chart s-name x-data y-data]
   (.addSeries chart s-name x-data y-data))
  ([chart s-name x-data y-data error-bars]
   (.addSeries chart s-name x-data y-data error-bars)))

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
  (if-let [font (fonts/->font font)]
    (-> style-map
        (dissoc style-map :font)
        (assoc-in-nonexisting [:axis :ticks :labels :font] font)
        (assoc-in-nonexisting [:axis :title :font] font)
        (assoc-in-nonexisting [:legend :font] font)
        (assoc-in-nonexisting [:annotations-font] font)
        (assoc-in-nonexisting [:chart :title :font] font))
    style-map))

(defprotocol Chart
  "Protocol for charts, which extends the XChart charts with
  additional polymorphic Clojure functions."
  (add-series! [chart series-name data]
    "A method to add new series to the provided chart"))
