(ns scribe.core
  (:import
   (java.io File)
   (java.awt Frame Dimension Graphics Graphics2D BasicStroke RenderingHints Color)
   (javax.swing JFrame JPanel JColorChooser JFileChooser JOptionPane JMenuBar JMenuItem JMenu UIManager)
   (javax.swing.event MouseInputAdapter)
   (javax.imageio ImageIO)
   (java.awt.event MouseEvent MouseListener KeyListener ActionListener ActionEvent KeyEvent)
   (java.awt.image BufferedImage)))

;;TODO: ADD OPTIONS
;;TODO: ADD HANDLING FOR TEXT
;;TODO: KEEP TRACK OF POINTS AND TEXT IN VECTOR. SAVE IN SPECIAL FORMAT.
;;TODO: ADD TOOLBAR!
;;TODO: SPLIT INTO RELEVANT SEPARATE FILES
;;TODO: ADD VARIABLE PEN WIDTH
;;TODO: THERE BE SOME RESIDUE WHEN COLOR IS CHANGED, OR SWITCHED TO ERASER

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

(def save-data (ref {:background (.getRGB Color/WHITE)
		     :points {};<COLOR> [coords] ;;COLOR IS RGB FORMAT. COORDS IS [[[0 1] [0 2]] [[12 1] [43 1]]... ]
		     :text []; => [{:color ** :font ** :text **} {:color ** :font ** :text **} ...]
		     }))

(defn add-coord [#^Color color line]
  (let [rgb (.getRGB color)
	bx (:x (line :b))
	by (:y (line :b))
	ex (:x (line :e))
	ey (:y (line :e))]
    (dosync (alter save-data assoc-in [:points rgb] (into (vec ((:points @save-data) rgb)) [[[bx by] [ex ey]]])))))

(defn repaint-points [#^Graphics g data]
 ; (.setBackground canvas (:background data)) ;DO THIS
  (.setStroke g (BasicStroke. 5.0))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
		     RenderingHints/VALUE_ANTIALIAS_ON)
  (let [points (reverse(:points data))] ;; reversed so layers are in correct order (LIFO -> FIFO)
    (doseq [[color coord] points]
      (.setColor g (Color. color))
      (loop [[f & r] coord]
	(let [[begin end] f]
	  (let [[bx by] begin
		[ex ey] end]
	    (.drawLine g bx by ex ey)))
	(when (not-empty r) (recur r))))))
 
(defn set-repaint? [val]
  (dosync (alter data assoc :repaint? val)))

(defn set-eraser? [val]
  (dosync (alter data assoc :eraser? val)))

(defn pick-color [key]
  (if-let [color (JColorChooser/showDialog nil "Choose a Color" Color/WHITE)]
    (dosync (alter data assoc key color))))

(defn save-image [save-file]
  (ImageIO/write screen "png"  save-file))

(defn dialog [message]
  (JOptionPane/showInputDialog message))

(defn save-dialog []
  (let [fc (JFileChooser.)]
    (if (= JFileChooser/APPROVE_OPTION (.showSaveDialog fc nil))
      (save-image (.getSelectedFile fc)))))

(defn draw-line [#^Graphics2D g line]
  (if (not (nil? line))
    (let [bx (:x (line :b))
	  by (:y (line :b))
	  ex (:x (line :e))
	  ey (:y (line :e))
	  color (:pen-color @data)]
      (add-coord color line)
      (doto g
	(.setColor color)
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

(def canvas (proxy [JPanel] [false ]
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

(defn repaint []
  (.repaint canvas))

(defn update-background [#^Color color]
  (.setBackground canvas color))
  
(defn reset [] ;;complete reset of options
  (dosync (alter data assoc
		 :last-point nil
		 :last-line nil
		 :last-string nil
		 :repaint? false
		 :screen-shot? false
		 :eraser? false
		 :eraser-size 15.0
		 :pen-size 5.0
		 :pen-color Color/BLACK
		 :background-color Color/WHITE))
;	  (alter save-data assoc :points {})) ;;REMOVE THIS ONCE TESTING IS DONE
  (update-background (:background-color @data))
  (set-repaint? true)
  (repaint))

(def menu-listener (proxy [ActionListener] []
		     (actionPerformed [#^ActionEvent e]
				      (let [source (.toLowerCase (.getText (.getSource e)))]
					(condp = source
					  "export" (save-dialog)
					  "new" (do (set-repaint? true) (repaint)))))))

(defn- create-item [name]
  (doto (JMenuItem. name)
    (.addActionListener menu-listener)))

(def menu-bar (doto (JMenuBar.)
		(.add (doto (JMenu. "File")
			(.add (create-item "Save"))
			(.add (create-item "New"))
			(.add (create-item "Export"))
			(.add (create-item "Load"))))
		(.add (doto (JMenu. "Edit")))))

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
		      (= key KeyEvent/VK_SPACE) (reset)
		      (= key KeyEvent/VK_E) (set-eraser? true)
		      (= key KeyEvent/VK_K) (repaint-points (.createGraphics screen) @save-data)
		      (= key KeyEvent/VK_A) (save-dialog)
		      (= key KeyEvent/VK_P) (pick-color :pen-color)
		      (= key KeyEvent/VK_Q) (do (pick-color :background-color) (update-background (:background-color @data)))
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