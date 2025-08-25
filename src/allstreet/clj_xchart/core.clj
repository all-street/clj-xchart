(ns allstreet.clj-xchart.core
  (:require [clojure.set :as set]
            [allstreet.clj-xchart.utils :as utils]
            [allstreet.clj-xchart.fonts :as fonts]
            [allstreet.clj-xchart.colors :as colors])
  (:import [org.knowm.xchart
            BubbleChart BubbleSeries$BubbleSeriesRenderStyle
            CategoryChart CategorySeries$CategorySeriesRenderStyle
            PieChart PieSeries$PieSeriesRenderStyle
            XYChart XYSeries$XYSeriesRenderStyle]
           [org.knowm.xchart.style
            AxesChartStyler AxesChartStyler$TextAlignment
            PieStyler$ClockwiseDirectionType
            PieStyler$LabelType Styler Styler$LegendPosition]
           [org.knowm.xchart.style.theme
            GGPlot2Theme MatlabTheme XChartTheme]
           [org.knowm.xchart.style.markers
            Circle Cross Diamond None None Oval Plus Rectangle
            Square Trapezoid TriangleDown TriangleUp]
           [org.knowm.xchart.style.lines SeriesLines]))

(def strokes
  "The default stroke types provided by XChart. You can also use a self-made
  stroke if you're not happy with any of the predefined ones."
  {:none SeriesLines/NONE
   :solid SeriesLines/SOLID
   :dash-dot SeriesLines/DASH_DOT
   :dash-dash SeriesLines/DASH_DASH
   :dot-dot SeriesLines/DOT_DOT})

(def markers
  "All the default XChart markers as keywords. To create your own marker, you
  must _subclass_ the org.knowm.xchart.style.markers.Marker class, so it's often better to use the default ones."
  {:circle (Circle.)
   :cross (Cross.)
   :diamond (Diamond.)
   :none (None.)
   :oval (Oval.)
   :plus (Plus.)
   :rectangle (Rectangle.)
   :square (Square.)
   :trapezoid (Trapezoid.)
   :triangle-down (TriangleDown.)
   :triangle-up (TriangleUp.)})

(def xy-render-styles
  "The different xy-render styles: :area, :line, :polygon-area, :scatter, :step, :step-area"
  {:area XYSeries$XYSeriesRenderStyle/Area
   :line XYSeries$XYSeriesRenderStyle/Line
   :polygon-area XYSeries$XYSeriesRenderStyle/PolygonArea
   :scatter XYSeries$XYSeriesRenderStyle/Scatter
   :step XYSeries$XYSeriesRenderStyle/Step
   :step-area XYSeries$XYSeriesRenderStyle/StepArea})

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

(def category-render-styles
  "The different styles you can use for category series."
  {:area CategorySeries$CategorySeriesRenderStyle/Area
   :bar CategorySeries$CategorySeriesRenderStyle/Bar
   :line CategorySeries$CategorySeriesRenderStyle/Line
   :scatter CategorySeries$CategorySeriesRenderStyle/Scatter
   :stepped-bar CategorySeries$CategorySeriesRenderStyle/SteppedBar
   :stick CategorySeries$CategorySeriesRenderStyle/Stick})

(def bubble-render-styles
  "Different render styles for bubble series. For now this is useless, as you
  can only use :round. Apparently :box is around the corner though."
  {:round BubbleSeries$BubbleSeriesRenderStyle/Round})

(def text-alignments
  "The different kinds of text alignments you can use."
  {:centre AxesChartStyler$TextAlignment/Centre
   :left AxesChartStyler$TextAlignment/Left
   :right AxesChartStyler$TextAlignment/Right})

(def legend-positions
  "The different legend positions. Note that xchart implements only a
  subset of inside/outside for the different positions."
  {:inside-n Styler$LegendPosition/InsideN
   :inside-ne Styler$LegendPosition/InsideNE
   :inside-nw Styler$LegendPosition/InsideNW
   :inside-s Styler$LegendPosition/InsideS
   :inside-se Styler$LegendPosition/InsideSE
   :inside-sw Styler$LegendPosition/InsideSW
   :outside-e Styler$LegendPosition/OutsideE
   :outside-s Styler$LegendPosition/OutsideS})

(def themes
  "The different default themes you can use with xchart."
  {:ggplot2 (GGPlot2Theme.)
   :matlab (MatlabTheme.)
   :xchart (XChartTheme.)})

(defmacro ^:private doto-cond
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

(defn- set-legend!
  [^Styler styler
   {:keys [background-color border-color font padding
           position series-line-length visible?]}]
  (doto-cond styler
    background-color (.setLegendBackgroundColor (colors/colors background-color background-color))
    border-color (.setLegendBorderColor (colors/colors border-color border-color))
    font (.setLegendFont (fonts/->font font))
    padding (.setLegendPadding (int padding))
    position (.setLegendPosition (legend-positions position))
    series-line-length (.setLegendSeriesLineLength (int series-line-length))
    (some? visible?) (.setLegendVisible (boolean visible?))))

(defn- set-chart-title-style!
  [^Styler styler
   {:keys [box font padding visible?]}]
  (let [{box-background-color :background-color
         box-border-color :color
         box-visible? :visible?} box]
    (doto-cond styler
      box-background-color (.setChartTitleBoxBackgroundColor (colors/colors box-background-color box-background-color))
      box-border-color (.setChartTitleBoxBorderColor (colors/colors box-border-color box-border-color))
      (some? box-visible?) (.setChartTitleBoxVisible (boolean box-visible?))
      font (.setChartTitleFont (fonts/->font font))
      padding (.setChartTitlePadding (int padding))
      (some? visible?) (.setChartTitleVisible (boolean visible?)))))

(defn- set-chart-style!
  [^Styler styler
   {:keys [background-color font-color padding title]}]
  (doto-cond styler
             background-color (.setChartBackgroundColor (colors/colors background-color background-color))
             font-color (.setChartFontColor (colors/colors font-color font-color))
             padding (.setChartPadding (int padding))
             title (set-chart-title-style! title)))

(defn- set-plot-style!
  [^Styler styler
   {:keys [background-color border-color border-visible? content-size]}]
  (doto-cond styler
             background-color (.setPlotBackgroundColor (colors/colors background-color background-color))
             border-color (.setPlotBorderColor (colors/colors border-color border-color))
             (some? border-visible?) (.setPlotBorderVisible (boolean border-visible?))
             content-size (.setPlotContentSize (double content-size))))

(defn- set-series-style!
  [^Styler styler series]
  ;; All of these are arrays, so we mutate them and set them back in.
  (let [series-colors (.getSeriesColors styler)
        series-lines (.getSeriesLines styler)
        series-markers (.getSeriesMarkers styler)
        series (vec series)]
    (dotimes [i (count series)]
      ;; TODO: nth instead mayhaps
      (let [{:keys [color stroke marker]} (series i)]
        (when color
          (aset series-colors i (colors/colors color color)))
        (when stroke
          (aset series-lines i (strokes stroke stroke)))
        (when marker
          (aset series-markers i (markers marker marker)))))
    (doto styler
      (.setSeriesColors series-colors)
      (.setSeriesLines series-lines)
      (.setSeriesMarkers series-markers))))

(defn- set-default-style!
  [^Styler styler
   {:keys [annotations-font _annotations? chart plot legend series]}]
  (doto-cond styler
    annotations-font (.setLabelsFont (fonts/->font annotations-font))
    ;; (some? annotations?) (.setHasAnnotations (boolean annotations?))
    chart (set-chart-style! chart)
    legend (set-legend! legend)
    plot (set-plot-style! plot)
    series (set-series-style! series)))

(defn- set-axis-ticks!
  [^AxesChartStyler styler
   {:keys [labels marks padding visible? line-visible?]}]
  (let [{:keys [color font]} labels]
    (doto-cond styler
      color (.setAxisTickLabelsColor (colors/colors color color))
      font (.setAxisTickLabelsFont (fonts/->font font))))
  (let [{:keys [length color stroke visible?]} marks]
    (doto-cond styler
      length (.setAxisTickMarkLength (int length))
      color (.setAxisTickMarksColor (colors/colors color color))
      stroke (.setAxisTickMarksStroke (strokes stroke stroke))
      (some? visible?) (.setAxisTicksMarksVisible (boolean visible?))))
  (doto-cond styler
    padding (.setAxisTickPadding (int padding))
    (some? line-visible?) (.setAxisTicksLineVisible (boolean line-visible?))
    (some? visible?) (.setAxisTicksVisible (boolean visible?))))

(defn- set-axis-title!
  [^AxesChartStyler styler
   {:keys [font visible? padding]}]
  (doto-cond styler
    font (.setAxisTitleFont (fonts/->font font))
    padding (.setAxisTitlePadding (int padding))
    (some? visible?) (.setAxisTitleVisible (boolean visible?))))

(defn- set-axis-plot!
  [^AxesChartStyler styler
   {:keys [grid-lines margin tick-marks?]}]
  (let [{:keys [horizontal? vertical? visible? color stroke]} grid-lines]
    (doto-cond styler
      (some? visible?) (.setPlotGridLinesVisible (boolean visible?))
      color (.setPlotGridLinesColor (colors/colors color color))
      stroke (.setPlotGridLinesStroke (strokes stroke stroke))
      (some? horizontal?) (.setPlotGridHorizontalLinesVisible (boolean horizontal?))
      (some? vertical?) (.setPlotGridVerticalLinesVisible (boolean vertical?))))
  (doto-cond styler
    margin (.setPlotMargin (int margin))
    (some? tick-marks?) (.setPlotTicksMarksVisible (boolean tick-marks?))))

(defn- set-x-axis-style!
  [^AxesChartStyler styler
   {:keys [label logarithmic? max min decimal-pattern
           tick-mark-spacing-hint ticks-visible? title-visible?]}]
  (let [{:keys [alignment rotation alignment-vertical]} label]
    (doto-cond styler
               alignment (.setXAxisLabelAlignment (text-alignments alignment alignment))
               alignment-vertical (.setXAxisLabelAlignmentVertical (text-alignments alignment-vertical))
               rotation (.setXAxisLabelRotation (int rotation))))
  (doto-cond styler
             decimal-pattern (.setXAxisDecimalPattern decimal-pattern)
             (some? logarithmic?) (.setXAxisLogarithmic (boolean logarithmic?))
             max (.setXAxisMax (double max))
             min (.setXAxisMin (double min))
             tick-mark-spacing-hint (.setXAxisTickMarkSpacingHint (int tick-mark-spacing-hint))
             (some? ticks-visible?) (.setXAxisTicksVisible (boolean ticks-visible?))
             (some? title-visible?) (.setXAxisTitleVisible (boolean title-visible?))))

(defn- set-y-axis-style!
  [^AxesChartStyler styler
   {:keys [label logarithmic? max min decimal-pattern
           tick-mark-spacing-hint ticks-visible? title-visible?]}]
  (let [{:keys [alignment _rotation]} label]
    (doto-cond styler
               alignment (.setYAxisLabelAlignment (text-alignments alignment alignment))))
  (doto-cond styler
             decimal-pattern (.setYAxisDecimalPattern decimal-pattern)
             (some? logarithmic?) (.setYAxisLogarithmic (boolean logarithmic?))
             max (.setYAxisMax (double max))
             min (.setYAxisMin (double min))
             tick-mark-spacing-hint (.setYAxisTickMarkSpacingHint (int tick-mark-spacing-hint))
             (some? ticks-visible?) (.setYAxisTicksVisible (boolean ticks-visible?))
             (some? title-visible?) (.setYAxisTitleVisible (boolean title-visible?))))

(defn- set-axes-style!
  [^AxesChartStyler styler
   {:keys [axis error-bars-color plot x-axis y-axis
           date-pattern decimal-pattern locale marker timezone]}]
  (let [ebc error-bars-color ;; error-bars-color is too long to be readable in these expressions
        {axis-ticks :ticks axis-title :title} axis
        {marker-size :size} marker]
    (doto-cond styler
      axis-ticks (set-axis-ticks! axis-ticks)
      axis-title (set-axis-title! axis-title)
      date-pattern (.setDatePattern date-pattern)
      decimal-pattern (.setDecimalPattern decimal-pattern)
      ;; The logic here is as follows: You can specify a colour for the error
      ;; bars. If the colour is :match-series, then the colour matches the series
      ;; colour, but if you specify something else, you cannot match the series!
      (and ebc (not= ebc :match-series)) (.setErrorBarsColor (colors/colors ebc ebc))
      (and ebc (not= ebc :match-series)) (.setErrorBarsColorSeriesColor false)
      (= ebc :match-series) (.setErrorBarsColorSeriesColor true)
      locale (.setLocale locale)
      marker-size (.setMarkerSize marker-size)
      plot (set-axis-plot! plot)
      timezone (.setTimezone timezone)
      x-axis (set-x-axis-style! x-axis)
      y-axis (set-y-axis-style! y-axis))))

(defn- add-raw-series
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


(defn- attach-default-font
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

(extend-type XYChart
  Chart
  (add-series! [chart s-name data]
    (if (sequential? data)
      (apply add-raw-series chart s-name data)
      (let [{:keys [x y error-bars style]} data
            {:keys [marker-color marker-type
                    line-color line-style line-width
                    fill-color show-in-legend? render-style]} style]
        (doto-cond (if error-bars
                     (add-raw-series chart s-name x y error-bars)
                     (add-raw-series chart s-name x y))
          render-style (.setXYSeriesRenderStyle (xy-render-styles render-style))
          marker-color (.setMarkerColor (colors/colors marker-color marker-color))
          marker-type (.setMarker (markers marker-type marker-type))
          line-color (.setLineColor (colors/colors line-color line-color))
          line-style (.setLineStyle (strokes line-style line-style))
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
         styling (attach-default-font styling)]
     (doto-cond (.getStyler chart)
                theme (.setTheme (themes theme theme))
                render-style (.setDefaultSeriesRenderStyle (xy-render-styles render-style)))
     (doseq [[s-name data] series]
       (add-series! chart s-name data))
     (doto (.getStyler chart)
       (set-default-style! styling)
       (set-axes-style! styling))
     (doto-cond chart
                title (.setTitle title)
                (-> styling :x-axis :title) (.setXAxisTitle (-> styling :x-axis :title))
                (-> styling :y-axis :title) (.setYAxisTitle (-> styling :y-axis :title))))))

(extend-type CategoryChart
  Chart
  (add-series! [chart s-name data]
    (if (sequential? data)
      (apply add-raw-series chart s-name data)
      (let [{:keys [x y error-bars style]} data
            {:keys [marker-color marker-type
                    line-color line-style line-width
                    fill-color show-in-legend? render-style]} style]
        (doto-cond (if error-bars
                     (add-raw-series chart s-name x y error-bars)
                     (add-raw-series chart s-name x y))
          render-style (.setChartCategorySeriesRenderStyle (category-render-styles render-style))
          marker-color (.setMarkerColor (colors/colors marker-color marker-color))
          marker-type (.setMarker (markers marker-type marker-type))
          line-color (.setLineColor (colors/colors line-color line-color))
          line-style (.setLineStyle (strokes line-style line-style))
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
         styling (attach-default-font styling)]
     (doseq [[s-name data] series]
       (add-series! chart s-name data))
     (doto-cond (.getStyler chart)
                theme (.setTheme (themes theme theme))
                render-style (.setDefaultSeriesRenderStyle (category-render-styles render-style))
                available-space-fill (.setAvailableSpaceFill (double available-space-fill))
                (some? overlap?) (.setOverlapped (boolean overlap?))
                (some? stacked?) (.setStacked (boolean stacked?)))
     (doto (.getStyler chart)
       (set-default-style! styling)
       (set-axes-style! styling))
     (doto-cond chart
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

(extend-type BubbleChart
  Chart
  (add-series! [chart s-name data]
    (if (sequential? data)
      (apply add-raw-series chart s-name data)
      (let [{:keys [x y bubble style]} data
            {:keys [marker-color marker-type
                    line-color line-style line-width
                    fill-color show-in-legend? render-style]} style]
        (doto-cond (add-raw-series chart s-name x y bubble)
          ;; NOTE: Add render style when squares are added to the impl?
          render-style (.setBubbleSeriesRenderStyle (bubble-render-styles render-style))
          line-color (.setLineColor (colors/colors line-color line-color))
          line-style (.setLineStyle (strokes line-style line-style))
          line-width (.setLineWidth (float line-width))
          fill-color (.setFillColor (colors/colors fill-color fill-color))
          (some? show-in-legend?) (.setShowInLegend (boolean show-in-legend?)))))))

(defn bubble-chart*
  "Returns a raw bubble chart. Bubble charts are hard to make right,
  so please see the tutorial for more information about how to create
  one. The render-styles page will give you information about styling
  options."
  ([series]
   (bubble-chart* series {}))
  ([series
    {:keys [width height title theme render-style]
     :or {width 640 height 500}
     :as styling}]
   {:pre [series]}
   (let [chart (BubbleChart. width height)
         styling (attach-default-font styling)]
     (doseq [[s-name data] series]
       (add-series! chart s-name data))
     (doto-cond (.getStyler chart)
                theme (.setTheme (themes theme theme))
                render-style (.setDefaultSeriesRenderStyle (bubble-render-styles render-style)))
     (doto (.getStyler chart)
       (set-default-style! styling)
       (set-axes-style! styling))
     (doto-cond chart
                title (.setTitle title)
                (-> styling :x-axis :title) (.setXAxisTitle (-> styling :x-axis :title))
                (-> styling :y-axis :title) (.setYAxisTitle (-> styling :y-axis :title))))))

(defn- max-bubble-value [series]
  (reduce max
          (mapcat :bubble (vals series))))

(defn- scale-bubbles
  "Scales the bubbles such that a bubble with size `in-val` has `out-val`
  diameter in pixels."
  [series in-val out-val]
  (let [bubble-fn #(* out-val (Math/sqrt (/ % in-val)))]
    (utils/map-vals
     (fn [data]
       (update data :bubble #(map bubble-fn %)))
     series)))

(defn bubble-chart
  ([series size]
   (bubble-chart series size {}))
  ([series {in :in [out-val out-type] :out :as bubble-size}
    {:keys [width height]
     :or {width 640 height 500}
     :as styling}]
   (let [ot ({:% :percent
              :percent :percent
              :px :pixels
              :pixels :pixels} out-type)
         out-size (if (identical? :percent ot)
                    (/ (* out-val (max width height))
                       100.0)
                    out-val)
         in (if (identical? in :max)
              (max-bubble-value series)
              in)]
     (when-not (and (number? in) (number? out-val) ot)
       (throw (ex-info "bubble-size is not in the correct format"
                       {:input bubble-size
                        :expected-example {:in 100 ;; or :max
                                           :out [100 :px]}})))
     (bubble-chart* (scale-bubbles series in out-size) styling))))

(defn- attach-default-annotation-distance
  "Attaches a default annotation distance if the donut thickness"
  [styling]
  (if-not (and (identical? :donut (:render-style styling))
               (not (:annotation-distance styling)))
    styling
    (assoc styling :annotation-distance
           (- 1.0 (/ (:donut-thickness styling 0.33) 2)))))

(extend-type PieChart
  Chart
  (add-series! [chart s-name data]
    (if (number? data)
      (.addSeries chart s-name data)
      (let [{:keys [style value]} num
            {:keys [render-style fill-color show-in-legend?]} style]
        (doto-cond (.addSeries chart s-name value)
          render-style (.setChartPieSeriesRenderStyle (pie-render-styles render-style))
          fill-color (.setFillColor (colors/colors fill-color fill-color))
          (some? show-in-legend?) (.setShowInLegend (boolean show-in-legend?)))))))

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
                     attach-default-font
                     attach-default-annotation-distance)
         ;; Need to rebind this one. We could probably omit it from the keys
         ;; entry at the top, if it's not used for documentation purposes.
         annotation-distance (:annotation-distance styling)]
     (doseq [[s-name data] series]
       (add-series! chart s-name data))
     (doto-cond (.getStyler chart)
                theme (.setTheme (themes theme theme))
                render-style (.setDefaultSeriesRenderStyle (pie-render-styles render-style))
                (some? circular?) (.setCircular (boolean circular?))
                (some? draw-all-annotations?) (.setDrawAllAnnotations (boolean draw-all-annotations?))
                annotation-distance (.setLabelsDistance (double annotation-distance))
                donut-thickness (.setDonutThickness (double donut-thickness))
                start-angle (.setStartAngleInDegrees (double start-angle))
                ;; annotation-type (.setAnnotationType (pie-annotation-types annotation-type))
                annotation-type (.setLabelType (pie-annotation-types annotation-type))
                direction-type (.setClockwiseDirectionType (pie-direction-types direction-type)))
     (set-default-style! (.getStyler chart) styling)
     (doto-cond chart
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
