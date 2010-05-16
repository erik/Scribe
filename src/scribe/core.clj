(ns scribe.core
  (:import
   (java.io File)
   (java.awt Frame Dimension Graphics Graphics2D BasicStroke RenderingHints Color)
   (javax.swing JFrame JPanel JColorChooser JOptionPane JMenuBar JMenuItem JMenu)
   (javax.swing.event MouseInputAdapter)
   (javax.imageio ImageIO)
   (java.awt.event MouseEvent MouseListener KeyListener ActionListener ActionEvent KeyEvent)
   (java.awt.image BufferedImage)))

;;TODO: ADD OPTIONS
;;TODO: ADD TOOLBAR, GUI ELEMENTS
;;TODO: SPLIT INTO RELEVANT SEPARATE FILES
;;TODO: ADD VARIABLE PEN WIDTH

(defstruct Point :x :y)
(defstruct Line :b :e)

(def save-dir (File. (str (System/getProperty "user.home") "/.scribe" )))
(def screen (BufferedImage. 800 600 BufferedImage/TYPE_INT_RGB))

(def data (ref {:last-point nil
		:last-line nil
		:last-string nil
		:repaint? false
		:screen-shot? false
		:eraser? false
		:eraser-size 15.0
		:pen-size 5.0
		:pen-color Color/BLACK
		:background-color Color/WHITE}))
 
(defn set-repaint? [val]
  (dosync (alter data assoc :repaint? val)))

(defn set-eraser? [val]
  (dosync (alter data assoc :eraser? val)))

(defn pick-color [key]
  (if-let [color (JColorChooser/showDialog nil "Choose a Color" Color/WHITE)]
    (dosync (alter data assoc key color))))

(defn dialog [message]
  (JOptionPane/showInputDialog message))

(defn draw-line [#^Graphics2D g line]
  (if (not (nil? line))
    (let [bx (:x (line :b))
	  by (:y (line :b))
	  ex (:x (line :e))
	  ey (:y (line :e))]
      (doto g
	(.setColor (:pen-color @data))
	(.setStroke (new BasicStroke 5.0)) 
	(.setRenderingHint RenderingHints/KEY_ANTIALIASING
			    RenderingHints/VALUE_ANTIALIAS_ON)
	(.drawLine bx by ex ey)))))

(defn draw-string [#^Graphics g]
  (.setColor g Color/BLACK)
  (.drawString g (:last-string @data) ((:last-point @data) :x) ((:last-point @data) :y)))

(defn erase [#^Graphics g]
  (let [line (:last-line @data)]
    (if (not (nil? line))
      (let [bx (:x (line :b))
	    by (:y (line :b))
	    ex (:x (line :e))
	    ey (:y (line :e))]
	(doto g
	  (.setColor Color/WHITE)
	  (.setStroke (new BasicStroke 15.0)) 
	  (.drawLine bx by ex ey))))))

;;GUI Components;;
    
(def frame (new JFrame "Scribe"))

(def canvas (proxy [JPanel] []
		   (paintComponent [#^Graphics g]
				   (let [gr (.createGraphics screen)]
				     (if (:eraser? @data)
				       (do
					 (erase gr))
				       (draw-line gr (:last-line @data)))
				     
				     (if (:repaint? @data)
				       (do
					 (proxy-super paintComponent gr)
					 (set-repaint? false)
					 (dosync (alter data assoc :last-line nil))))
				     
				     (if (and
					  (not (nil? (:last-point @data)))
					  (not (nil? (:last-string @data))))				       
				       (do
					 (draw-string gr)
					 (dosync (alter data assoc :last-string nil))))
				     (.drawImage g screen 0 0 nil)))))

(def menu-listener (proxy [ActionListener] []
		     (actionPerformed [#^ActionEvent e]
				      (let [source (.getText (.getSource e))](println source)))))

(def menu-bar (doto (JMenuBar.)
		;;BOILERPLATE FTL :(
		;;MAKE A CREATE-MENUITEM FUNC WHICH DOES THIS FOR YOU!
		(.add (doto (JMenu. "File")
			(.add (doto (JMenuItem. "Save")
				(.addActionListener menu-listener)))
			(.add (doto (JMenuItem. "New")
				(.addActionListener menu-listener)))
			(.add (doto (JMenuItem. "Export")
				(.addActionListener menu-listener)))
			(.add (doto (JMenuItem. "Load")
				(.addActionListener menu-listener)))))
		(.add (doto (JMenu. "Edit")))))
     
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
			(alter data assoc :last-point (struct Point ex ey))))
		     (repaint))
       
       (mouseDragged [#^MouseEvent e]
		     (if (not (nil? (:last-point @data)))
		       (let [bx ((:last-point @data) :x)
			     by ((:last-point @data) :y)
			     ex (.getX e)
			     ey (.getY e)]
			 (dosync
			  (alter data assoc :last-point (struct Point ex ey))
			  (alter data assoc :last-line (struct Line (struct Point bx by) (:last-point @data))))
			 (repaint))))
       
       (mouseReleased [#^MouseEvent e])))

(def key-handle
     (proxy [KeyListener] []
       (keyTyped [#^KeyEvent e])
       (keyPressed [#^KeyEvent e]
		   (let [key (.getKeyCode e)]
		     (cond
		      (= key KeyEvent/VK_SPACE) (set-repaint? true)
		      (= key KeyEvent/VK_E) (set-eraser? true)
		      (= key KeyEvent/VK_A) (save-image "screen")
		      (= key KeyEvent/VK_P) (pick-color :pen-color)
		      (= key KeyEvent/VK_W) (set-eraser? false)
		      (= key KeyEvent/VK_ESCAPE) (System/exit 0)
		      (= key KeyEvent/VK_S) (dosync
					     (alter data assoc :last-string  (dialog "Enter some text"))
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
    (.setJMenuBar menu-bar)
    (.setBackground Color/WHITE)
    (.setSize 800 600)
    (.add canvas)
    (.addKeyListener key-handle)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setResizable false)
    (.pack)
    (.setVisible true)
    (.setResizable false)))
