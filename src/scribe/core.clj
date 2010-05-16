(ns scribe.core
  (:import
   (java.io File)
   (java.awt Frame Dimension Graphics Graphics2D BasicStroke RenderingHints Color)
   (javax.swing JFrame JPanel)
   (javax.swing.event MouseInputAdapter)
   (javax.imageio ImageIO)
   (java.awt.event MouseEvent MouseListener KeyListener KeyEvent)
   (java.awt.image BufferedImage)))

;;TODO: ADD OPTIONS
;;TODO: SPLIT INTO RELEVANT SEPARATE FILES
;;TODO: ADD VARIABLE PEN WIDTH

(defstruct Point :x :y)
(defstruct Line :b :e)


;;CHANGE TO A SINGLE REF
(def last-point (ref nil))
(def last-line (ref nil))
(def last-string (ref nil))
(def repaint? (ref false))
(def screen-shot? (ref false))
(def eraser? (ref false))
(def save-dir (File. (str (System/getProperty "user.home") "/.scribe" )))

(def screen (BufferedImage. 800 600 BufferedImage/TYPE_INT_RGB))
 
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
	(.setColor Color/BLACK)
	(.setStroke (new BasicStroke 5.0)) 
	(.setRenderingHint RenderingHints/KEY_ANTIALIASING
			    RenderingHints/VALUE_ANTIALIAS_ON)
	(.drawLine bx by ex ey)))))

(defn draw-string [#^Graphics g]
  (.setColor g Color/BLACK)
  (.drawString g @last-string (@last-point :x) (@last-point :y)))

(defn erase [#^Graphics g]
  (let [line @last-line]
    (if (not (nil? line))
      (let [bx (:x (line :b))
	    by (:y (line :b))
	    ex (:x (line :e))
	    ey (:y (line :e))]
	(doto g
	  (.setColor Color/WHITE)
	  (.setStroke (new BasicStroke 15.0)) 
	  (.drawLine bx by ex ey))))))
    
(def frame (new JFrame "Scribe"))

(def canvas (proxy [JPanel] []
		   (paintComponent [#^Graphics g]
				   (let [gr (.createGraphics screen)]
				     (if @eraser?
				       (do
					 (erase gr))
				       (draw-line gr @last-line))
				     
				     (if @repaint?
				       (do
					 (proxy-super paintComponent gr)
					 (set-repaint? false)
					 (dosync (ref-set last-line nil))))
				     
				     (if (and
					  (not (nil? @last-point))
					  (not (nil? @last-string)))
				       
				       (do
					 (draw-string gr)
					 (dosync (ref-set last-string nil))))
				     (.drawImage g screen 0 0 nil)))))
(defn save-image [name]
  (ImageIO/write screen "png" (File. (str save-dir "/" name  ".png"))))

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
		      (= key KeyEvent/VK_A) (save-image "screen")
		      (= key KeyEvent/VK_W) (set-eraser? false)
		      (= key KeyEvent/VK_ESCAPE) (System/exit 0)
		      (= key KeyEvent/VK_S) (dosync
					     (ref-set last-string  (dialog))
					     (.requestFocus frame)))
		     (repaint)))
       (keyReleased [#^KeyEvent e])))

(defn scribe-window []
  (set-repaint? true)
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
