(ns scribe.core
  (:import (java.awt Frame Dimension Graphics Graphics2D BasicStroke RenderingHints Color)
	   (javax.swing JFrame JPanel)
	   (javax.swing.event MouseInputAdapter)
	   (java.awt.event MouseEvent MouseListener KeyListener KeyEvent)))

;;TODO: ADD OPTIONS, SAVE ABILITY
;;TODO: SPLIT INTO RELEVANT SEPARATE FILES
;;TODO: ADD VARIABLE PEN WIDTH
;;TODO: ADD ERASER

;;USE CLJ_CONFIG TO SAVE

(defstruct Point :x :y)
(defstruct Line :b :e)


;;CHANGE TO A SINGLE REF
(def last-point (ref nil))
(def last-line (ref nil))
(def last-string (ref nil))
(def repaint? (ref false))
(def eraser? (ref false))

(defn set-repaint? [val]
  (dosync (ref-set repaint? val)))

(defn set-eraser? [val]
  (dosync (ref-set eraser? val)))

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

(defn draw-string [#^Graphics g]
  (.drawString g @last-string (@last-point :x) (@last-point :y)))

(defn erase [#^Graphics g]
  (doto g
    (.setColor Color/WHITE)
    (.fillRect (@last-point :x) (@last-point :y) 15 15)))

(def frame (new JFrame "Scribe"))

(def canvas (proxy [JPanel] []
		   (paintComponent [#^Graphics g]
				   (if @eraser?
				     (do
				       (erase g))
				     (draw-line g @last-line))
				   
				   (if @repaint?
				     (do
				       (proxy-super paintComponent g)
				       (set-repaint? false)
				       (dosync (ref-set last-line nil))))
				   
				   (if (and
					(not (nil? @last-point))
					(not (nil? @last-string)))
					    
				     (do
				       (draw-string g)
				       (dosync (ref-set last-string nil)))))))

(defn repaint []
  (.repaint canvas))

(def mouse-handle
     (proxy [MouseInputAdapter] []
       
       (mousePressed [#^MouseEvent e]
		     (dosync
		      (let [ex (.getX e)
			    ey (.getY e)]
			(ref-set last-point (struct Point ex ey))))
		     (repaint))
       
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
(defn- dialog []
  (javax.swing.JOptionPane/showInputDialog "What's your message?"))
(def key-handle
     (proxy [KeyListener] []
       (keyTyped [#^KeyEvent e])
       (keyPressed [#^KeyEvent e]
		   (let [key (.getKeyCode e)]
		     (cond
		      (= key KeyEvent/VK_SPACE) (set-repaint? true)
		      (= key KeyEvent/VK_E) (set-eraser? true)
		      (= key KeyEvent/VK_W) (set-eraser? false)
		      (= key KeyEvent/VK_ESCAPE) (System/exit 0)
		      (= key KeyEvent/VK_S) (dosync
					     (ref-set last-string  (dialog))
					     (.requestFocus frame)))
		     (repaint)))
       (keyReleased [#^KeyEvent e])))

(defn scribe-window []
  (doto canvas
    (.setBackground Color/WHITE)
    (.setPreferredSize (new Dimension 800 600))
    (.addMouseMotionListener  mouse-handle)
    (.addMouseListener mouse-handle))
  (doto frame
    (.setBackground Color/WHITE)
    (.setSize 800 600)
    (.add canvas)
    (.addKeyListener key-handle)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setResizable false)
    (.pack)
    (.setVisible true)
    (.setResizable false)))
