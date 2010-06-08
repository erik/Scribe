(ns scribe.core
  (:use [clojure.contrib.duck-streams :only [spit slurp*]])
  (:import
   (java.io File)
   (java.awt Frame Dimension Graphics Graphics2D BasicStroke RenderingHints Color Font)
   (javax.swing JFrame JPanel JSeparator JButton JToggleButton JToolBar JColorChooser JFileChooser JOptionPane JMenuBar JMenuItem JMenu UIManager KeyStroke)
   (javax.swing.event MouseInputAdapter)
   (javax.imageio ImageIO)
   (java.awt.event MouseEvent MouseListener KeyListener ActionListener ActionEvent KeyEvent)
   (java.awt.image BufferedImage)))

;;TODO: MULTILINE DIALOG BOX
;;TODO: REPAINT-POINTS IS MINE FIELD OF BOILER PLATE
;;TODO: ADD OPTIONS

(defstruct Point :x :y)
(defstruct Line :b :e)

(def save-dir (File. (str (System/getProperty "user.home") "/.scribe" )))
(def #^BufferedImage screen (BufferedImage. 800 600 BufferedImage/TYPE_INT_RGB))

(def data (ref {:last-point nil
		:last-line nil
		:last-string nil
		:repaint? false
		:screen-shot? false
		:eraser? false
		:eraser-size 15.0
		:text-size 12
		:pen-size 5.0
		:pen-color Color/BLACK
		:background-color Color/WHITE}))


(def save-data (ref {:background (.getRGB Color/WHITE)
		     :eraser-points []
		     :points {};<COLOR> [coords] ;;COLOR IS RGB FORMAT. COORDS IS [[[0 1] [0 2]] [[12 1] [43 1]]... ]
		     :text []; => [{:color ** :size ** :text **} {:color ** :size ** :text **} ...]
		     }))


(defn draw-string 
  ([#^Graphics2D g {:keys [#^String text #^Integer x #^Integer y color size]}]
     (.setColor g (Color. color))
     (.setFont g (Font. "sansserrif" Font/PLAIN size))
       (loop [[#^String f & r] (.split text  "\n|\r\n")
	      y y]
	 (.drawString g #^String f x y)
	 (when (seq r) (recur r (+ y size)))))
  ([#^Graphics2D g]
     (draw-string g {:text (:last-string @data)
		     :x ((:last-point @data) :x)
		     :y ((:last-point @data) :y)
		     :size 12
		     :color (.getRGB (Color/BLACK))})))

(defn add-coord [#^Color color line]
  (let [rgb (.getRGB color)
	bx (:x (line :b))
	by (:y (line :b))
	ex (:x (line :e))
	ey (:y (line :e))]
    (dosync (alter save-data assoc-in [:points rgb] (into (vec ((:points @save-data) rgb)) [[[bx by] [ex ey]]])))))

(defn add-eraser-coord [line]
  (let [bx (:x (line :b))
	by (:y (line :b))
	ex (:x (line :e))
	ey (:y (line :e))]
    (dosync (alter save-data assoc :eraser-points (conj (:eraser-points @save-data) [[[bx by] [ex ey]]])))))

(defn add-string [{:keys [color size text x y]}]
  (dosync (alter save-data assoc :text (conj (:text @save-data) {:color color :size size :text text :x x :y y}))))

(defn repaint-points [#^Graphics2D g data]
  ;;LOTS O' BOILER PLATE HERE!
  (.setStroke g (BasicStroke. 5.0))
  (.setRenderingHint g RenderingHints/KEY_ANTIALIASING
		     RenderingHints/VALUE_ANTIALIAS_ON)
  (let [points (reverse (:points data))] ;; reversed so layers are in correct order (LIFO -> FIFO)
    (doseq [[color coord] points]
      (.setColor g (Color. color))
      (loop [[f & r] coord]
	(let [[begin end] f]
	  (let [[bx by] begin
		[ex ey] end]
	    (.drawLine g bx by ex ey)))
	(when (not-empty r) (recur r)))))

  (doseq [map (:text @save-data)]
    (let [color (:color map)
	  size (:size map)
	  text (:text map)
	  x (:x map)
	  y (:y map)]
      (draw-string g map)))
	  
  (.setStroke g (BasicStroke. 15.0))
  (let [color (:background @save-data)]
    (.setColor g (Color. color))
    (doseq [coord (:eraser-points @save-data)]
      (loop [[f & r] coord]
   	(let [[begin end] f]
	  (let [[bx by] begin
		[ex ey] end]
	    (.drawLine g bx by ex ey)))
	(when (not-empty r) (recur r))))))
       
(defn set-repaint? [val]
  (dosync (alter data assoc :repaint? val)))

(defn set-eraser? [val]
  (dosync (alter data assoc :eraser? val :last-point nil :last-line nil)))

(defn pick-color [key]
  (if-let [color (JColorChooser/showDialog nil "Choose a Color" Color/WHITE)]
    (dosync (alter data assoc key color))))

(defn save-image [#^File save-file]
  (when save-file (ImageIO/write #^BufferedImage screen "png"  save-file)))

(defn dialog [message]
  (JOptionPane/showInputDialog message))

(defn message [message]
  (JOptionPane/showMessageDialog nil message))
  
(defn save-dialog
  ([parent]
     (let [fc (JFileChooser.)]
       (if (= JFileChooser/APPROVE_OPTION (.showSaveDialog fc parent))
	 (.getSelectedFile fc))))
  ([]
     (save-dialog nil)))

(defn load-dialog
  ([parent]
     (let [fc (JFileChooser.)]
       (if (= JFileChooser/APPROVE_OPTION (.showOpenDialog fc parent))
	 (.getSelectedFile fc))))
  ([]
     (load-dialog nil)))

(defn draw-line
  ([#^Graphics2D g line line-size]
     (if (not (nil? line))
       (let [bx (:x (line :b))
	     by (:y (line :b))
	     ex (:x (line :e))
	     ey (:y (line :e))
	     color (:pen-color @data)]
	 (add-coord color line)
	 (doto g
	   (.setColor color)
	   (.setStroke (new BasicStroke line-size)) 
	   (.setRenderingHint RenderingHints/KEY_ANTIALIASING
			      RenderingHints/VALUE_ANTIALIAS_ON)
	   (.drawLine bx by ex ey)))))
  ([#^Graphics2D g line]
     (draw-line g line 5.0)))
 

(defn erase [#^Graphics2D g]
  (let [line (:last-line @data)]
    (if (not (nil? line))
      (let [bx (:x (line :b))
	    by (:y (line :b))
	    ex (:x (line :e))
	    ey (:y (line :e))]
	(add-eraser-coord line)
	(doto g
	  (.setColor #^Color (:background-color @data))
	  (.setStroke (BasicStroke. 15.0)) 
	  (.drawLine bx by ex ey))))))

;;GUI Components;;
    
(def frame (new JFrame "Scribe"))

(def canvas (proxy [JPanel] [true]
		   (paintComponent [#^Graphics g]
				   (let [#^Graphics2D gr (.createGraphics screen)]
				     (if (:eraser? @data)
				       (do
					 (erase gr))
				       (draw-line gr (:last-line @data)))
				     
				     (if (:repaint? @data)
				       (do
					 (proxy-super paintComponent gr)
					 (repaint-points gr @save-data)
					 (set-repaint? false)
					 (dosync (alter data assoc :last-line nil))))
				     (let [last-point (:last-point @data)
					   last-string (:last-string @data)
					   #^Color color (:pen-color @data)
					   size (:text-size @data)]
				       
				       (if (and
					    (seq last-point)
					    (seq last-string))
					 (do
					   (add-string {:color (.getRGB color)
							:size size
							:text last-string
							:x (last-point :x)
							:y (last-point :y)})
					   
					   (draw-string gr {:color (.getRGB color)
							    :size 12
							    :text (:last-string @data)
							    :x (last-point :x)
							    :y (last-point :y)})
					   (dosync (alter data assoc :last-string nil)))))
				     (.drawImage g screen 0 0 nil)))))

(defn repaint []
  (.repaint #^JPanel canvas))

(defn update-background [#^Color color]
  (dosync (alter save-data assoc :background (.getRGB color)))
  (set-repaint? true)
  (.setBackground #^JPanel canvas color)
  (.requestFocus #^JFrame frame)
  (repaint))

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
		 :text-size 12.0
		 :pen-color Color/BLACK
		 :background-color Color/WHITE)
	  (alter save-data assoc
		 :background nil
		 :points {}
		 :text []
		 :eraser-points []))
  (update-background (:background-color @data))
  (set-repaint? true)
  (repaint))

(defn read-file []
  (reset)
  (if-let [file (load-dialog frame)]
    (do
      (try
	(let [content (read-string (slurp* file))]
	  (when-not (map? content)
	    (throw (Exception. "LOL, BOOM")))
	  (dosync
	   (ref-set save-data content)
	   (alter data assoc :background-color (Color. (:background @save-data)))))
	(update-background (:background-color @data))
	(repaint-points (.createGraphics screen) @save-data)
	(repaint)
	(catch Exception e
	  (println e)
	  (message "File is corrupt, or not a Scribe file"))))))

(def button-listener (proxy [ActionListener] []
		       (actionPerformed [#^ActionEvent e]
					(let [cmd (.getActionCommand e)]
					  (condp = cmd
					      "export" (save-image (save-dialog frame))
					      "save" (if-let [file (save-dialog frame)]
						       (spit file  (str @save-data)))
					      "open" (read-file)
					      "background" (do
							     (pick-color :background-color)
							     (update-background (:background-color @data)))
					      "pick-color" (pick-color :pen-color)
					      "eraser" (set-eraser? (not (:eraser? @data)))
					      "new" (reset))))))
					    
(defn make-button [text action-command tool-tip]
  (doto (JButton.)
    (.setActionCommand action-command)
    (.setToolTipText tool-tip)
    (.setText text)
    (.addActionListener button-listener)))

(defn make-toggle-button [text action-command tool-tip]
  (doto (JToggleButton. text false)
    (.setActionCommand action-command)
    (.setToolTipText tool-tip)
    (.addActionListener button-listener)))

(def toolbar
   
     (doto (JToolBar.)
       (.setFloatable false)
       (.add (make-button "Save" "save" "Save your sketch"))
       (.add (make-button "Export" "export" "Export your sketch to PNG image"))
       (.add (make-button "Open" "open" "Open an existing sketch"))
       (.add (make-button "New" "new" "Discard current sketch and create a new one"))
       (.add (javax.swing.JToolBar$Separator.))
       (.add (make-toggle-button "Eraser" "eraser" "Toggle eraser"))
       (.add (make-button "Choose Color" "pick-color" "Choose the color, for both pen and brush"))
       (.add (make-button "Change Background" "background" "Change the background color"))))
  
(def menu-listener (proxy [ActionListener] []
		     (actionPerformed [#^ActionEvent e]
				      (let [#^String source (.toLowerCase #^String (.getText #^JMenuItem (.getSource e)))]
					(condp = source  
					  "export as png" (save-image (save-dialog frame))
					  "save" (if-let [file (save-dialog frame)]
						   (spit file  (str @save-data)))
					  "open" (read-file)
					  "new" (reset))))))
;;FORMAT FOR KEYSTROKES:
;    <modifiers>* (<typedID> | <pressedReleasedID>)
;
;    modifiers := shift | control | ctrl | meta | alt | altGraph
;    typedID := typed <typedKey>
;    typedKey := string of length 1 giving Unicode character.
;    pressedReleasedID := (pressed | released) key
;    key := KeyEvent key code name, i.e. the name following "VK_".


(defn- create-item
  ([#^String name #^String key]
     (doto (JMenuItem. name)
       (.setAccelerator #^KeyStroke (KeyStroke/getKeyStroke key))
       (.addActionListener #^ActionListener menu-listener)))
  ([name]
     (create-item name "")))

(def menu-bar (doto (#^JMenuBar JMenuBar.)
		(.add (doto (#^JMenu JMenu. "File")
			(.add #^JMenuItem (create-item "Save" "ctrl S"))
			(.add #^JMenuItem (create-item "New" "ctrl N"))
			(.add #^JMenuItem (create-item "Export as PNG" "ctrl E"))
			(.add #^JMenuItem (create-item "Open" "ctrl O"))))
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
		      (= key KeyEvent/VK_P) (pick-color :pen-color)
		      (= key KeyEvent/VK_Q) (do
					      (pick-color :background-color)
					      (update-background (:background-color @data)))
		      (= key KeyEvent/VK_W) (set-eraser? false)
		      (= key KeyEvent/VK_ESCAPE) (System/exit 0)
		      (= key KeyEvent/VK_D) (dosync
					     (alter data assoc :last-string (dialog "Enter some text"))
					     (.requestFocus #^JFrame frame)))
		     (repaint)))
       (keyReleased [#^KeyEvent e])))

(defn scribe-window []
  (set-repaint? true)
  (doto #^JPanel canvas
    (.setBackground Color/WHITE)
    (.setPreferredSize (new Dimension 800 600))
    (.addMouseMotionListener  mouse-handle)
    (.addMouseListener mouse-handle))
  (doto #^JFrame frame
    (.setJMenuBar menu-bar)
    (.setBackground Color/WHITE)
    (.setSize 800 600)
    (.add toolbar java.awt.BorderLayout/PAGE_START)
    (.add #^JPanel canvas)
    (.addKeyListener key-handle)
    (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
    (.setResizable false)
    (.pack)
    (.setVisible true)
    (.setResizable false)))