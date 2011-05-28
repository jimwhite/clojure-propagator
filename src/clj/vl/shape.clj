(ns
  #^{:author "Michael T. Nygard <mtnygard@gmail.com>"
     :doc "Shape data structures, functions, and some basic shapes"}
  vl.shape
  (:import [javax.swing JFrame JPanel BorderFactory]
           [java.awt Color Dimension Graphics Graphics2D]
           [java.awt.event ComponentListener])
  (:use  [clojure.contrib swing-utils]
         [vl graph]))

(defstruct shape :x :y :width :height :text :font :text-align :text-valign)
(def shape-defaults (struct-map shape :width 40 :height 26 :text-align 0.5 :text-valign 0.5 ))

(defn text-in-space
  ([shape space-k extent alignment-k] (text-in-space (shape space-k) extent (get shape alignment-k 0)))
  ([space extent alignment] (* alignment (- space extent))))

(defn text-origin [s tw th]
  [(text-in-space s :width  tw :text-align)
   (text-in-space s :height th :text-valign)])

(defn bbox
  ([shape] (bbox (:x shape) (:y shape) (:width shape) (:height shape)))
  ([x y w h] [x y (+ x w) (+ y h)]))

(defn hit-bbox?
  [[xl yt xr yb] [px py]] (and (<= xl px xr) (<= yt py yb)))

(defn hit?
  [v [px py]] (hit-bbox? (bbox v) [px py]))

(defn center
  ([start extent] (+ start (/ extent 2)))
  ([shape] [(center (:x shape) (:width  shape))
            (center (:y shape) (:height shape))]))

(defmacro save-transform [#^Graphics2D g & body]
  `(let [t# (.getTransform ~g)]
    (do ~@body)
    (.setTransform ~g t#)))

(defn draw-edge
  "Draw an edge on a Graphics panel."
  [#^Graphics g vf vt s]
  (let [[startx starty] (center vf)
        [endx endy] (center vt)]
    (.setColor g (or (:color vt) (:edge-color s)))
    (.drawLine g startx starty endx endy)))

(defn draw-edges
  "Draw all the edges from a vertex on a Graphics panel."
  [#^Graphics g vf neighbors s]
  (doseq [vt neighbors]
    (draw-edge g vf vt s)))

(defn draw-text [#^Graphics2D g v]
  (if (:text v)
    (let [s (:text v)
          fm (.getFontMetrics g)
          textrect (.getStringBounds fm s g)
          [textx texty] (text-origin v (.width textrect) (.height textrect))]
      (.drawString g s (+ (:x v) (int textx)) (+ (:y v) (int texty) (.getAscent fm))))))

(defn draw-vertex
  "Draw a vertex on a Graphics panel."
  [#^Graphics2D g v s]
  (save-transform g
    (.translate g (:x v) (:y v)))
    (.setColor g
             (or (:fill-color v)
                 (:vertex-fill-color s)))
    (.fillOval g (:x v) (:y v) (:width v) (:height v))
    (.setColor g
             (or (:outline-color v)
                 (:vertex-outline-color s)))
    (.drawOval g (:x v) (:y v) (:width v) (:height v))
    (draw-text g v)
  )

(defn draw-graph
  "Draw a graph on a Graphics panel.
  The graph and settings are assumed to be refs."
  [#^Graphics g graph settings]
  (println "drawing graph")
  (let [graph-snapshot @graph     ; grab a snapshot of the graph
        s              @settings] ; grab a snapshot of the settings
    (doseq [n (get-nodes graph-snapshot)]
      ;; draw the edges first so they don't show on top of the vertices
      (draw-edges g n (get-neighbors graph-snapshot n) s))
    (doseq [n (get-nodes graph-snapshot)]
      (draw-vertex g n s))))

(defn make-shape [m]
  (into shape-defaults m))
