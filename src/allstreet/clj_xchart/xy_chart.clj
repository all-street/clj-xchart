(ns allstreet.clj-xchart.xy-chart
  (:require
   [allstreet.clj-xchart.colors :as colors]
   [allstreet.clj-xchart.common :as common]
   [allstreet.clj-xchart.fonts :as fonts]
   [allstreet.clj-xchart.lines :as lines]
   [allstreet.clj-xchart.markers :as markers]
   [allstreet.clj-xchart.style :as style]
   [allstreet.clj-xchart.theme :as theme])
  (:import [org.knowm.xchart XYChart XYSeries$XYSeriesRenderStyle]))

(def xy-render-styles
  "The different xy-render styles: :area, :line, :polygon-area, :scatter, :step, :step-area"
  {:area XYSeries$XYSeriesRenderStyle/Area
   :line XYSeries$XYSeriesRenderStyle/Line
   :polygon-area XYSeries$XYSeriesRenderStyle/PolygonArea
   :scatter XYSeries$XYSeriesRenderStyle/Scatter
   :step XYSeries$XYSeriesRenderStyle/Step
   :step-area XYSeries$XYSeriesRenderStyle/StepArea})

(extend-type XYChart
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
          render-style (.setXYSeriesRenderStyle (xy-render-styles render-style))
          marker-color (.setMarkerColor (colors/colors marker-color marker-color))
          marker-type (.setMarker (markers/markers marker-type marker-type))
          line-color (.setLineColor (colors/colors line-color line-color))
          line-style (.setLineStyle (lines/strokes line-style line-style))
          line-width (.setLineWidth (float line-width))
          fill-color (.setFillColor (colors/colors fill-color fill-color))
          (some? show-in-legend?) (.setShowInLegend (boolean show-in-legend?)))))))

(defn xy-chart
  "Returns an xy-chart. See the tutorial for more information about
  how to create an xy-chart, and see the render-styles documentation
  for styling options."
  ([series]
   (xy-chart series {}))
  ([series
    {:keys [width height title theme render-style]
     :or {width 640 height 500}
     :as styling}]
   {:pre [series]}
   (let [chart (XYChart. width height)
         styling (fonts/attach-default-font styling)]
     (common/doto-cond (.getStyler chart)
       theme (.setTheme (theme/themes theme theme))
       render-style (.setDefaultSeriesRenderStyle (xy-render-styles render-style)))
     (doseq [[s-name data] series]
       (common/add-series! chart s-name data))
     (doto (.getStyler chart)
       (style/set-default-style! styling)
       (style/set-axes-style! styling))
     (common/doto-cond chart
       title (.setTitle title)
       (-> styling :x-axis :title) (.setXAxisTitle (-> styling :x-axis :title))
       (-> styling :y-axis :title) (.setYAxisTitle (-> styling :y-axis :title))))))

(comment
  (import 'java.util.GregorianCalendar)

  (defn months [year]
    (map (fn [month]
           (let [c (GregorianCalendar.)]
             (.set c year month 1)
             (.getTime c)))
         (range 12)))
  ;; Circle Cross Diamond None None Oval Plus Rectangle
  ;; Square Trapezoid TriangleDown TriangleUp
  (require '[allstreet.clj-xchart.view :as v])
  (v/view
   (xy-chart
    {"Wins" {:x (months 2015)
             :y [0 2 3 3 4 7 7 8 8 7 7 5]
             :style {:marker-type :rectangle
                     :render-style :step-area
                     :marker-color :black
                     :line-color :green}}
     "Losses" {:x (months 2015)
               :y [3 2 2 0 2 4 3 1 3 4 2 0]
               :style {:marker-type :triangle-down
                       :render-style :area
                       :marker-color :black
                       :line-color :red}}}
    {:title "Wins and Losses in 2015"
     :date-pattern "MMM"}))
  :comment)
