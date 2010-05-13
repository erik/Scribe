(ns scribe.core
  (:import (java.awt Frame Dimension Graphics)
	   (javax.swing JFrame JPanel)
	   (javax.swing.event MouseInputAdapter)
	   (java.awt.event MouseMotionAdapter MouseEvent MouseListener MouseAdapter MouseWheelListener MouseWheelEvent)))

;;TODO: MAKE STUFF DISPLAY IN RIGHT SPOT!

;;for REPL use only!
;(add-classpath "file:/usr/share/java/jogl.jar")
;(add-classpath "file:/usr/share/java/gluegen-rt.jar")


(defstruct Point :x :y)
(defstruct Line :b :e)

(def last-point (ref nil))

(def lines (ref []))

(defn draw-line [#^Graphics g lines] 
  (if (seq lines)
    (let [line (first lines)]
      (if (not (nil? line))
	(let [bx (:x (line :b))
	      by (:y (line :b))
	      ex (:x (line :e))
	      ey (:y (line :e))]
	  (doto g
	    (.drawLine bx by ex ey)))))))
;	  (draw-line g (rest lines)))))))

(defn draw-lines [#^Graphics g]
  (draw-line g @lines))

(def canvas (proxy [JPanel] []
	      (paintComponent [#^Graphics g]
			      (proxy-super paintComponent g)
			      (draw-lines g))))

(def mouse-handle
     (proxy [MouseInputAdapter] []
       
       (mousePressed [#^MouseEvent e]
		     (dosync (ref-set last-point (struct Point (.getX e) (.getY e)))))
       
       (mouseDragged [#^MouseEvent e]
		     (if (not (nil? @last-point))
		       (let [x (@last-point :x)
			     y (@last-point :y)]
			 (dosync
			  (ref-set last-point (struct Point (.getX e) (.getY e)))
			  (alter lines into [(struct Line (struct Point x y)  @last-point)]))
			 (.repaint canvas))))
       
       (mouseReleased [#^MouseEvent e])))


(defn scribe-window []
  (let [frame (new JFrame "Scribe")]
    (.addMouseMotionListener canvas mouse-handle)
    (.addMouseListener canvas mouse-handle)
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (. canvas (setPreferredSize (new Dimension 800 600)))
    (.. frame (getContentPane) (add canvas))
    (doto frame
      (.setSize 800 600)
      (.pack)
      (.setVisible true))))
