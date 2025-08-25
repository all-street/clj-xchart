(ns allstreet.clj-xchart.theme
  (:import [org.knowm.xchart.style.theme
            GGPlot2Theme MatlabTheme XChartTheme]))

(def themes
  "The different default themes you can use with xchart."
  {:ggplot2 (GGPlot2Theme.)
   :matlab (MatlabTheme.)
   :xchart (XChartTheme.)})
