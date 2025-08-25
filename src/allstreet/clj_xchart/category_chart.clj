(ns allstreet.clj-xchart.category-chart
  (:require [clojure.set :as set]
            [allstreet.clj-xchart.utils :as utils]
            [allstreet.clj-xchart.style :as style]
            [allstreet.clj-xchart.fonts :as fonts]
            [allstreet.clj-xchart.theme :as theme]
            [allstreet.clj-xchart.colors :as colors]
            [allstreet.clj-xchart.markers :as markers]
            [allstreet.clj-xchart.common :as common]
            [allstreet.clj-xchart.lines :as lines])
  (:import [org.knowm.xchart
            CategoryChart CategorySeries$CategorySeriesRenderStyle]))

(def category-render-styles
  "The different styles you can use for category series."
  {:area CategorySeries$CategorySeriesRenderStyle/Area
   :bar CategorySeries$CategorySeriesRenderStyle/Bar
   :line CategorySeries$CategorySeriesRenderStyle/Line
   :scatter CategorySeries$CategorySeriesRenderStyle/Scatter
   :stepped-bar CategorySeries$CategorySeriesRenderStyle/SteppedBar
   :stick CategorySeries$CategorySeriesRenderStyle/Stick})


(extend-type CategoryChart
  common/Chart
  (add-series! [chart s-name data]
    (if (sequential? data)
      (apply common/add-raw-series chart s-name data)
      (let [{:keys [x y error-bars style]} data
            {:keys [marker-color marker-type
                    line-color line-style line-width
                    fill-color show-in-legend? render-style]} style]
        (common/doto-cond (if error-bars
                            (common/add-raw-series chart s-name x y error-bars)
                            (common/add-raw-series chart s-name x y))
          render-style (.setChartCategorySeriesRenderStyle (category-render-styles render-style))
          marker-color (.setMarkerColor (colors/colors marker-color marker-color))
          marker-type (.setMarker (markers/markers marker-type marker-type))
          line-color (.setLineColor (colors/colors line-color line-color))
          line-style (.setLineStyle (lines/strokes line-style line-style))
          line-width (.setLineWidth (float line-width))
          fill-color (.setFillColor (colors/colors fill-color fill-color))
          (some? show-in-legend?) (.setShowInLegend (boolean show-in-legend?)))))))

(defn category-chart*
  "Returns a raw category chart. Prefer `category-chart` unless you
  run into performance issues. See the tutorial for more information
  about how to create category charts, and see the render-styles
  documentation for styling options."
  ([series]
   (category-chart* series {}))
  ([series
    {:keys [width height title theme render-style available-space-fill overlap?
            stacked?]
     :or {width 640 height 500}
     :as styling}]
   {:pre [series]}
   (let [chart (CategoryChart. width height)
         styling (fonts/attach-default-font styling)]
     (doseq [[s-name data] series]
       (common/add-series! chart s-name data))
     (common/doto-cond (.getStyler chart)
       theme (.setTheme (theme/themes theme theme))
       render-style (.setDefaultSeriesRenderStyle (category-render-styles render-style))
       available-space-fill (.setAvailableSpaceFill (double available-space-fill))
       (some? overlap?) (.setOverlapped (boolean overlap?))
       (some? stacked?) (.setStacked (boolean stacked?)))
     (doto (.getStyler chart)
       (style/set-default-style! styling)
       (style/set-axes-style! styling))
     (common/doto-cond chart
       title (.setTitle title)
       (-> styling :x-axis :title) (.setXAxisTitle (-> styling :x-axis :title))
       (-> styling :y-axis :title) (.setYAxisTitle (-> styling :y-axis :title))))))

;; Utility functions

(defn- normalize-category-series
  "Returns the content of a category series on the shape {:x ... :y ...} with
  styling data retained."
  [series-data]
  (cond (and (map? series-data)
             (contains? series-data :x)
             (contains? series-data :y)) series-data
        (and (map? series-data)
             (contains? series-data :content)) (-> (:content series-data)
                                                   (normalize-category-series)
                                                   ;; retain styling data:
                                                   (merge (dissoc series-data :content)))
        ;; Assuming keys are strings/vals
        (and (map? series-data)
             (every? (comp not keyword?)
                     (keys series-data))) {:x (keys series-data)
                                           :y (vals series-data)}
        (sequential? series-data) {:x (first series-data)
                                   :y (second series-data)}))

(defn- category-series-xs
  "Given a map of series, return all the unique x-elements as a set."
  [series]
  (->> (vals series)
       (mapcat :x)
       set))

(defn- reorder-series
  "Reorders a normalized series content to the assigned ordering"
  [{:keys [x y] :as series} x-order]
  ;; Here we may unfortunately recompute an input value. If perfomance is an
  ;; issue, we may attach the mapping onto the series.
  (let [mapping (zipmap x y)]
    (assoc series
           :x x-order
           :y (mapv (fn [x] (get mapping x 0.0)) x-order))))

;; I do have some issues differing between a single series and multiple series.
;; I'll call a map of series a series-map for now.
(defn- normalize-category-series-map
  "Given a series map, normalize the series to contain all x values with the
  order specified in x-order. If the x value does not exist in a series, the
  value 0.0 is inserted. If there are other x values not in x-order, they are
  attached at the end in sorted order."
  [series-map x-order]
  (let [series-map (utils/map-vals normalize-category-series series-map)
        x-order (vec x-order)
        extra-xs (sort (set/difference (category-series-xs series-map)
                                       (set x-order)))
        x-order (into x-order extra-xs)]
    (utils/map-vals #(reorder-series % x-order) series-map)))

(defn category-chart
  "Returns a category chart. See the tutorial for more information
  about how to create category charts, and see the render-styles
  documentation for styling options."
  ([series]
   (category-chart series {}))
  ([series {:keys [x-axis series-order] :as styling}]
   (let [x-order (:order x-axis)
         normalized-map (normalize-category-series-map series x-order)
         extra-categories (->> (apply dissoc normalized-map series-order)
                               (sort-by key))
         normalized-seq (concat (keep #(find normalized-map %) series-order)
                                extra-categories)]
     (category-chart* normalized-seq styling))))

(comment
  (require '[allstreet.clj-xchart.view :as v])
  (v/view
   (category-chart
    {"Bananas" {"Mon" 6, "Tue" 2, "Fri" 3, "Wed" 1, "Thur" 3}
     "Apples" {"Tue" 3, "Wed" 5, "Fri" 1, "Mon" 1}
     "Pears" {"Thur" 1, "Mon" 3, "Fri" 4, "Wed" 1}}
    {:title "Weekly Fruit Sales"
     ;; :theme :matlab
     :legend {:position :inside-n}
     :font {:style :italic}
     :render-style :bar
     :x-axis {:order ["Mon" "Tue" "Wed" "Thur" "Fri"]}}))
  )
