(ns allstreet.clj-xchart.colors
  (:import [java.awt Color]))

(def colors
  "All the default java.awt colors as keywords. You can use this map
  to iterate over the keys, in case you'd like to compare different
  colors. Or you could use java.awt.Color directly to use the exact
  color you want."
  {:black Color/BLACK
   :blue Color/BLUE
   :cyan Color/CYAN
   :dark-gray Color/DARK_GRAY
   :gray Color/GRAY
   :green Color/GREEN
   :light-gray Color/LIGHT_GRAY
   :magenta Color/MAGENTA
   :orange Color/ORANGE
   :pink Color/PINK
   :red Color/RED
   :white Color/WHITE
   :yellow Color/YELLOW})
