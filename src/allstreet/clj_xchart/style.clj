(ns allstreet.clj-xchart.style
  (:require
   [allstreet.clj-xchart.fonts :as fonts]
   [allstreet.clj-xchart.common :as common]
   [allstreet.clj-xchart.colors :as colors]
   [allstreet.clj-xchart.lines :as lines]
   [allstreet.clj-xchart.markers :as markers])
  (:import
   [org.knowm.xchart.style
    AxesChartStyler AxesChartStyler$TextAlignment
    Styler Styler$LegendPosition]))

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

(defn- set-legend!
  [^Styler styler
   {:keys [background-color border-color font padding
           position series-line-length visible?]}]
  (common/doto-cond styler
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
    (common/doto-cond styler
      box-background-color (.setChartTitleBoxBackgroundColor (colors/colors box-background-color box-background-color))
      box-border-color (.setChartTitleBoxBorderColor (colors/colors box-border-color box-border-color))
      (some? box-visible?) (.setChartTitleBoxVisible (boolean box-visible?))
      font (.setChartTitleFont (fonts/->font font))
      padding (.setChartTitlePadding (int padding))
      (some? visible?) (.setChartTitleVisible (boolean visible?)))))

(defn- set-chart-style!
  [^Styler styler
   {:keys [background-color font-color padding title]}]
  (common/doto-cond styler
    background-color (.setChartBackgroundColor (colors/colors background-color background-color))
    font-color (.setChartFontColor (colors/colors font-color font-color))
    padding (.setChartPadding (int padding))
    title (set-chart-title-style! title)))

(defn- set-plot-style!
  [^Styler styler
   {:keys [background-color border-color border-visible? content-size]}]
  (common/doto-cond styler
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
          (aset series-lines i (lines/strokes stroke stroke)))
        (when marker
          (aset series-markers i (markers/markers marker marker)))))
    (doto styler
      (.setSeriesColors series-colors)
      (.setSeriesLines series-lines)
      (.setSeriesMarkers series-markers))))

(defn set-default-style!
  [^Styler styler
   {:keys [annotations-font _annotations? chart plot legend series]}]
  (common/doto-cond styler
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
    (common/doto-cond styler
      color (.setAxisTickLabelsColor (colors/colors color color))
      font (.setAxisTickLabelsFont (fonts/->font font))))
  (let [{:keys [length color stroke visible?]} marks]
    (common/doto-cond styler
      length (.setAxisTickMarkLength (int length))
      color (.setAxisTickMarksColor (colors/colors color color))
      stroke (.setAxisTickMarksStroke (lines/strokes stroke stroke))
      (some? visible?) (.setAxisTicksMarksVisible (boolean visible?))))
  (common/doto-cond styler
    padding (.setAxisTickPadding (int padding))
    (some? line-visible?) (.setAxisTicksLineVisible (boolean line-visible?))
    (some? visible?) (.setAxisTicksVisible (boolean visible?))))

(defn- set-axis-title!
  [^AxesChartStyler styler
   {:keys [font visible? padding]}]
  (common/doto-cond styler
    font (.setAxisTitleFont (fonts/->font font))
    padding (.setAxisTitlePadding (int padding))
    (some? visible?) (.setAxisTitleVisible (boolean visible?))))

(defn- set-axis-plot!
  [^AxesChartStyler styler
   {:keys [grid-lines margin tick-marks?]}]
  (let [{:keys [horizontal? vertical? visible? color stroke]} grid-lines]
    (common/doto-cond styler
      (some? visible?) (.setPlotGridLinesVisible (boolean visible?))
      color (.setPlotGridLinesColor (colors/colors color color))
      stroke (.setPlotGridLinesStroke (lines/strokes stroke stroke))
      (some? horizontal?) (.setPlotGridHorizontalLinesVisible (boolean horizontal?))
      (some? vertical?) (.setPlotGridVerticalLinesVisible (boolean vertical?))))
  (common/doto-cond styler
    margin (.setPlotMargin (int margin))
    (some? tick-marks?) (.setPlotTicksMarksVisible (boolean tick-marks?))))

(defn- set-x-axis-style!
  [^AxesChartStyler styler
   {:keys [label logarithmic? max min decimal-pattern
           tick-mark-spacing-hint ticks-visible? title-visible?]}]
  (let [{:keys [alignment rotation alignment-vertical]} label]
    (common/doto-cond styler
      alignment (.setXAxisLabelAlignment (text-alignments alignment alignment))
      alignment-vertical (.setXAxisLabelAlignmentVertical (text-alignments alignment-vertical))
      rotation (.setXAxisLabelRotation (int rotation))))
  (common/doto-cond styler
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
    (common/doto-cond styler
      alignment (.setYAxisLabelAlignment (text-alignments alignment alignment))))
  (common/doto-cond styler
    decimal-pattern (.setYAxisDecimalPattern decimal-pattern)
    (some? logarithmic?) (.setYAxisLogarithmic (boolean logarithmic?))
    max (.setYAxisMax (double max))
    min (.setYAxisMin (double min))
    tick-mark-spacing-hint (.setYAxisTickMarkSpacingHint (int tick-mark-spacing-hint))
    (some? ticks-visible?) (.setYAxisTicksVisible (boolean ticks-visible?))
    (some? title-visible?) (.setYAxisTitleVisible (boolean title-visible?))))

(defn set-axes-style!
  [^AxesChartStyler styler
   {:keys [axis error-bars-color plot x-axis y-axis
           date-pattern decimal-pattern locale marker timezone]}]
  (let [ebc error-bars-color ;; error-bars-color is too long to be readable in these expressions
        {axis-ticks :ticks axis-title :title} axis
        {marker-size :size} marker]
    (common/doto-cond styler
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
