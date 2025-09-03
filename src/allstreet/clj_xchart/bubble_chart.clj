(ns allstreet.clj-xchart.bubble-chart
  (:require
   [allstreet.clj-xchart.colors :as colors]
   [allstreet.clj-xchart.common :as common]
   [allstreet.clj-xchart.fonts :as fonts]
   [allstreet.clj-xchart.lines :as lines]
   [allstreet.clj-xchart.style :as style]
   [allstreet.clj-xchart.theme :as theme]
   [allstreet.clj-xchart.utils :as utils])
  (:import [org.knowm.xchart
            BubbleChart BubbleSeries$BubbleSeriesRenderStyle]))

(def bubble-render-styles
  "Different render styles for bubble series. For now this is useless, as you
  can only use :round. Apparently :box is around the corner though."
  {:round BubbleSeries$BubbleSeriesRenderStyle/Round})

(extend-type BubbleChart
  common/Chart
  (add-series! [chart s-name data]
    (if (sequential? data)
      (apply common/add-raw-series chart s-name data)
      (let [{:keys [x y bubble style]} data
            {:keys [_marker-color _marker-type
                    line-color line-style line-width
                    fill-color show-in-legend? render-style]} style]
        (common/doto-cond (common/add-raw-series chart s-name x y bubble)
          ;; NOTE: Add render style when squares are added to the impl?
          render-style (.setBubbleSeriesRenderStyle (bubble-render-styles render-style))
          line-color (.setLineColor (colors/colors line-color line-color))
          line-style (.setLineStyle (lines/strokes line-style line-style))
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
         styling (fonts/attach-default-font styling)]
     (doseq [[s-name data] series]
       (common/add-series! chart s-name data))
     (common/doto-cond (.getStyler chart)
       theme (.setTheme (theme/themes theme theme))
       render-style (.setDefaultSeriesRenderStyle (bubble-render-styles render-style)))
     (doto (.getStyler chart)
       (style/set-default-style! styling)
       (style/set-axes-style! styling))
     (common/doto-cond chart
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


(comment
  (require '[allstreet.clj-xchart.view :as v])
  (v/view
   (bubble-chart
    {"A" {:x [1.5 2.6 3.3 4.9 5.5 6.3 1 2.0 3.0 4.0 5 6]
          :y [10 4 7 7.7 7 5.5 10 4 7 1 7 9]
          :bubble [17 40 50 51 26 20 66 35 80 27 29 44]}
     "B" {:x [1 2.0 3.0 4.0 5 6 1.5 2.6 3.3 4.9 5.5 6.3]
          :y [1 2 3 4 5 6 10 8.5 4 1 4.7 9]
          :bubble [37 35 80 27 29 44 57 40 50 33 26 20]
          }}
    {:in 100 :out [100 :px]
     :render-style :round
     :line-color :blue
     :line-style :dash-dot
     :line-width 10
     :fill-color :yellow
     :show-in-legend? true}))

  :comment)
