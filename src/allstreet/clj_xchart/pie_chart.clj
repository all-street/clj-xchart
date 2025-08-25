(ns allstreet.clj-xchart.pie-chart
  (:require
   [allstreet.clj-xchart.common :as common]
   [allstreet.clj-xchart.colors :as colors])
  (:import [org.knowm.xchart
            PieChart PieSeries$PieSeriesRenderStyle]
           [org.knowm.xchart.style
            PieStyler$ClockwiseDirectionType PieStyler$LabelType]))

(def pie-render-styles
  "The different pie render styles. It is :pie by default."
  {:pie PieSeries$PieSeriesRenderStyle/Pie
   :donut PieSeries$PieSeriesRenderStyle/Donut})

(def pie-annotation-types
  "The different annotation types you can use to annotate pie charts.
  By default, this is :percentage."
  {:value PieStyler$LabelType/Value
   :percentage PieStyler$LabelType/Percentage
   :label PieStyler$LabelType/Name
   :label-and-percentage PieStyler$LabelType/NameAndPercentage
   :label-and-value PieStyler$LabelType/NameAndValue})

(def pie-direction-types
  "The different direction types or pie charts."
  {:clockwise PieStyler$ClockwiseDirectionType/CLOCKWISE
   :counter-clockwise PieStyler$ClockwiseDirectionType/COUNTER_CLOCKWISE})


(extend-type PieChart
  common/Chart
  (add-series! [chart s-name data]
    (if (number? data)
      (.addSeries chart s-name data)
      (let [{:keys [style value]} num
            {:keys [render-style fill-color show-in-legend?]} style]
        (common/doto-cond (.addSeries chart s-name value)
                          render-style (.setChartPieSeriesRenderStyle (pie-render-styles render-style))
                          fill-color (.setFillColor (colors/colors fill-color fill-color))
                          (some? show-in-legend?) (.setShowInLegend (boolean show-in-legend?)))))))

(defn attach-default-annotation-distance
  "Attaches a default annotation distance if the donut thickness"
  [styling]
  (if-not (and (identical? :donut (:render-style styling))
               (not (:annotation-distance styling)))
    styling
    (assoc styling :annotation-distance
           (- 1.0 (/ (:donut-thickness styling 0.33) 2)))))


(defn pie-chart
  "Returns a pie chart. The series map is in this case just a mapping
  from string to number. For styling information, see the
  render-styles page.

  Example:
  (c/pie-chart {\"Red\" 54
                \"Green\" 34})"
  ([series]
   (pie-chart series {}))
  ([series
    {:keys [width height title circular? theme render-style _annotation-distance
            start-angle draw-all-annotations? donut-thickness annotation-type direction-type]
     :or {width 640 height 500}
     :as styling}]
   {:pre [series]}
   (let [chart (PieChart. width height)
         styling (-> styling
                     common/attach-default-font
                     attach-default-annotation-distance)
         ;; Need to rebind this one. We could probably omit it from the keys
         ;; entry at the top, if it's not used for documentation purposes.
         annotation-distance (:annotation-distance styling)]
     (doseq [[s-name data] series]
       (common/add-series! chart s-name data))
     (common/doto-cond (.getStyler chart)
                       theme (.setTheme (common/themes theme theme))
                       render-style (.setDefaultSeriesRenderStyle (pie-render-styles render-style))
                       (some? circular?) (.setCircular (boolean circular?))
                       (some? draw-all-annotations?) (.setDrawAllAnnotations (boolean draw-all-annotations?))
                       annotation-distance (.setLabelsDistance (double annotation-distance))
                       donut-thickness (.setDonutThickness (double donut-thickness))
                       start-angle (.setStartAngleInDegrees (double start-angle))
                       ;; annotation-type (.setAnnotationType (pie-annotation-types annotation-type))
                       annotation-type (.setLabelType (pie-annotation-types annotation-type))
                       direction-type (.setClockwiseDirectionType (pie-direction-types direction-type)))
     (common/set-default-style! (.getStyler chart) styling)
     (common/doto-cond chart
                       title (.setTitle title)
                       (-> styling :x-axis :title) (.setXAxisTitle (-> styling :x-axis :title))
                       (-> styling :y-axis :title) (.setYAxisTitle (-> styling :y-axis :title))))))

(comment
  (require '[allstreet.clj-xchart.view :as v])
  (v/view
   (pie-chart
    {":none" 845
     ":simple" 371
     ":whitespace" 303
     ":advanced" 1013}
    {:title (str "Which ClojureScript optimization "
                 "settings do you use?")
     :font {:name :serif}
     :legend {:position :inside-ne}
     :render-style :donut
     :annotation-type :label-and-value
     :direction-type :counter-clockwise
     :annotation-distance 0.82}))
  :comment)
