(ns allstreet.clj-xchart.markers
  (:import
   [org.knowm.xchart.style.markers
    Circle Cross Diamond None None Oval Plus Rectangle
    Square Trapezoid TriangleDown TriangleUp]))

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
