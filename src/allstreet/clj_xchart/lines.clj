(ns allstreet.clj-xchart.lines
  (:import [org.knowm.xchart.style.lines SeriesLines]))

(def strokes
  "The default stroke types provided by XChart. You can also use a self-made
  stroke if you're not happy with any of the predefined ones."
  {:none SeriesLines/NONE
   :solid SeriesLines/SOLID
   :dash-dot SeriesLines/DASH_DOT
   :dash-dash SeriesLines/DASH_DASH
   :dot-dot SeriesLines/DOT_DOT})
