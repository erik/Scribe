(ns scribe.core
  (:import (java.awt Frame Dimension Graphics Graphics2D BasicStroke RenderingHints Color)
	   (javax.swing JFrame JPanel)
	   (javax.swing.event MouseInputAdapter)
	   (java.awt.event  MouseEvent MouseListener KeyListener KeyEvent)))

;;TODO: ADD COMMAND ABILITIES!
;;TODO: REMOVE RESIDUE (CLEAN LAST POINT)


(defstruct Point :x :y)
(defstruct Line :b :e)

(def last-point (ref nil))
(def last-line (ref nil))

(def repaint? (ref false))

(defn set-repaint? [val]
  (dosync (ref-set repaint? val)))

(defn draw-line [#^Graphics2D g line]
  (if (not (nil? line))
    (let [bx (:x (line :b))
	  by (:y (line :b))
	  ex (:x (line :e))
	  ey (:y (line :e))]
      (doto g
	(.setStroke (new BasicStroke 5.0))
	(.setRenderingHint RenderingHints/KEY_ANTIALIASING
			    RenderingHints/VALUE_ANTIALIAS_ON)
	(.drawLine bx by ex ey)))))

(def canvas (proxy [JPanel] []
		   (paintComponent [#^Graphics g]
				   (when @repaint? (do
						     (proxy-super paintComponent g)
						     (set-repaint? false)))
				   (draw-line g @last-line))))

(defn repaint []
  (.repaint canvas))

(def mouse-handle
     (proxy [MouseInputAdapter] []
       
       (mousePressed [#^MouseEvent e]
		     (dosync
		      (let [ex (.getX e)
			    ey (.getY e)]
			(ref-set last-point (struct Point ex ey))))
		     (.repaint canvas))
       
       (mouseDragged [#^MouseEvent e]
		     (if (not (nil? @last-point))
		       (let [bx (@last-point :x)
			     by (@last-point :y)
			     ex (.getX e)
			     ey (.getY e)]
			 (dosync
			  (ref-set last-point (struct Point ex ey))
			  (ref-set last-line (struct Line (struct Point bx by) @last-point)))
			 (repaint))))
       
       (mouseReleased [#^MouseEvent e])))

(def key-handle
     (proxy [KeyListener] []
       (keyTyped [#^KeyEvent e]
		 (let [key (.getKeyChar e)]
		   (condp = key
		     \space (do
			      (set-repaint? true)
			      (repaint)))))
       (keyPressed [#^KeyEvent e])
       (keyReleased [#^KeyEvent e])))

(defn scribe-window []
  (let [frame (new JFrame "Scribe")]
    (.addMouseMotionListener canvas mouse-handle)
    (.addMouseListener canvas mouse-handle)
    (.addKeyListener frame key-handle)
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (. canvas (setPreferredSize (new Dimension 800 600)))
    (.. frame (getContentPane) (add canvas))
    (doto frame
      (.setSize 800 600)
      (.pack)
      (.setVisible true))))
