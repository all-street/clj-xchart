(ns allstreet.clj-xchart.render
  (:refer-clojure :exclude [spit])
  (:require [clojure.string :as str])
  (:import [org.knowm.xchart
            BitmapEncoder BitmapEncoder$BitmapFormat
            VectorGraphicsEncoder VectorGraphicsEncoder$VectorGraphicsFormat]
           [java.io FileOutputStream ByteArrayOutputStream]))

(defn as-buffered-image
  "Converts a chart into a java.awt.image.BufferedImage."
  [chart]
  (BitmapEncoder/getBufferedImage chart))

(def ^:private bitmap-formats
  {:png BitmapEncoder$BitmapFormat/PNG
   :gif BitmapEncoder$BitmapFormat/GIF
   :bmp BitmapEncoder$BitmapFormat/BMP
   :jpg BitmapEncoder$BitmapFormat/JPG
   :jpeg BitmapEncoder$BitmapFormat/JPG})

(def ^:private vector-formats
  {:pdf VectorGraphicsEncoder$VectorGraphicsFormat/PDF
   :svg VectorGraphicsEncoder$VectorGraphicsFormat/SVG
   :eps VectorGraphicsEncoder$VectorGraphicsFormat/EPS})

(defn to-bytes
  "Converts a chart into a byte array."
  ([chart type]
   (if-let [bitmap-format (bitmap-formats type)]
     (BitmapEncoder/getBitmapBytes chart bitmap-format)
     (if-let [vector-format (vector-formats type)]
       (let [baos (ByteArrayOutputStream.)]
         (VectorGraphicsEncoder/saveVectorGraphic chart baos vector-format)
         (.getBytes baos))
       (throw (IllegalArgumentException. (str "Unknown format: " type)))))))

(defn- guess-extension
  [fname]
  (when-let [last-dot (str/last-index-of fname ".")]
    (let [extension (str/lower-case (subs fname (inc last-dot)))]
      (keyword extension))))

(defn spit
  "Spits the chart to the given filename. If no type is provided, the type is
  guessed by the filename extension. If no extension is found, an error is
  raised."
  ([chart fname]
   (spit chart fname (guess-extension fname)))
  ([chart fname type]
   (with-open [fos (FileOutputStream. fname)]
     (.write fos (to-bytes chart type)))))
