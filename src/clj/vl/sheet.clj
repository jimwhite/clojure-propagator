(ns
    #^{:author "Michael T. Nygard <mtnygard@gmail.com>"
       :doc "Visual modeling and editing"}
  vl.sheet
  (:import [javax.swing JFrame JPanel BorderFactory]
           [java.awt Color Dimension Font Graphics RenderingHints]
           [java.awt.event ComponentListener MouseAdapter])
  (:use [clojure.contrib swing-utils]
        [vl shape graph]))
                          
(defstruct view-settings
  :width
  :height
  :background-color
  :vertex-fill-color
  :vertex-diameter
  :vertex-outline-color
  :edge-color
  :text-font)

(def default-settings
  (struct-map view-settings
    :width 480
    :height 320
    :vertex-diameter 10
    :background-color Color/WHITE
    :vertex-fill-color Color/WHITE
    :vertex-outline-color Color/GRAY
    :edge-color Color/BLACK
    :text-font (Font. "Gill Sans" 0 14)))

(defn add-mouse-listener
  [component f & args]
  (let [listener (proxy [MouseAdapter] []
    (mouseClicked [event] (apply f event args)))]
    (.addMouseListener component listener)
    listener))

(defn fill-shape [gr node color]
  (alter gr replace-node node (assoc node :fill-color color)))

(defn fill-shapes [gr nodes color]
  (doseq [n nodes] #(fill-shape gr n color)))

(defn nodes-in-color [nodes color]
  (map #(assoc % :fill-color color) nodes))

(defn double-click? [event]
  (= 2 (.getClickCount event)))

(defn mouse-point [event]
  [(.. event getPoint x) (.. event getPoint y)])

(defn recolor-nodes
  "Set color of nodes that match the predicate"
  [gr pred color]
  (let [matches (filter pred (get-nodes @gr))
        news (nodes-in-color matches color)]
    (alter gr replace-nodes matches news)
    news))

(defn make-selection-tool [gr settings]
  (let [sel (ref nil)]
    (fn [e & _]
      (dosync
        (if (.isShiftDown e)
          (alter sel conj (recolor-nodes gr #(hit? % (mouse-point e)) Color/CYAN))
          (do
            (recolor-nodes gr (fn [n] true) (:vertex-fill-color settings))
            (ref-set sel (recolor-nodes gr #(hit? % (mouse-point e)) Color/CYAN)))
          )))))

(defn sheet-panel-proxy
  "Create a proxy JPanel"
  [graph settings]
  (proxy [JPanel] []
    (getPreferredSize []
                      (Dimension. (settings :width) (settings :height)))
    (paintComponent [g]
                    (proxy-super paintComponent g)
                    (.setRenderingHint g RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
                    (draw-graph g graph settings))))

(defn sheet-panel
  "Create a panel to display a sheet of shapes"
  [graph tool settings]
  (let [panel (sheet-panel-proxy graph settings)]
    (add-watch graph
               :shape-repaint
               (fn [key ref old-state new-state]
                 (.repaint panel)))
    (add-watch settings
               :settings-repaint
               (fn [key ref old-state new-state]
                 (.setBackground panel (:background-color new-state))
                 (.repaint panel)))
    (doto panel
      (.setBorder (BorderFactory/createLineBorder Color/BLACK))
      (.setBackground (:background-color @settings))
      (add-mouse-listener @tool))))


(defn add-component-listener
  [component f & args]
  (let [listener (proxy [ComponentListener] []
                   (componentResized [event] (apply f event args))
                   (componentMoved [event] (apply f event args))
                   (componentShown [event] (apply f event args)))]
    (.addComponentListener component listener)
    listener))

(defn sheet-frame
  [graph tool settings]
  (let [frame (JFrame. "Sheet View")]
    (add-watch settings
               :settings-set-size
               (fn [key ref old-state new-state]
                 (println "resized")
                 (.setSize frame (:width new-state) (:height new-state))))
    (doto frame
      (add-component-listener (fn [e]
                                (let [c (.getSource e)]
                                  (dosync
                                   (alter settings assoc :width (.getWidth c) :height (.getHeight c))))))
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.add (sheet-panel graph tool settings))
      (.pack)
      (.setLocationRelativeTo nil))))

(defn open-frame
  ([graph]
    (open-frame graph default-settings))
  ([graph local-settings]
    (let [settings (ref (merge default-settings local-settings))
          tool (ref (make-selection-tool graph settings))
          frame (sheet-frame graph tool settings)]
      (do-swing
        (javax.swing.UIManager/setLookAndFeel (javax.swing.UIManager/getSystemLookAndFeelClassName))
        (.setVisible frame true))
      frame)))


;;; Quick start
(comment
  (use '(vl shape sheet graph sample) 'example.vl.sample)
  (def gr (ref (make-sample-graph)))
  (def sheet (open-frame gr))
)
