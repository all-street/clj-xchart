(ns allstreet.clj-xchart.core
  "Backwards compatibility namespace"
  (:require
   [allstreet.clj-xchart.view :as view]
   [allstreet.clj-xchart.bubble-chart :as bubble-chart]
   [allstreet.clj-xchart.category-chart :as category-chart]
   [allstreet.clj-xchart.pie-chart :as pie-chart]
   [allstreet.clj-xchart.xy-chart :as xy-chart]))

(def view view/view)

(def bubble-chart bubble-chart/bubble-chart)
(def category-chart category-chart/category-chart)
(def pie-chart pie-chart/pie-chart)
(def xy-chart xy-chart/xy-chart)
