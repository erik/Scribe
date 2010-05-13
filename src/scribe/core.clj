(ns scribe.core
  (:import (java.awt Frame Dimension)
	   (javax.swing JFrame)
	   (javax.swing.event MouseInputAdapter)
	   (java.awt.event MouseMotionAdapter MouseEvent MouseListener MouseAdapter MouseWheelListener MouseWheelEvent)
	   (javax.media.opengl GLCanvas GLEventListener GL GLAutoDrawable)
	   (javax.media.opengl.glu GLU)
	   (com.sun.opengl.util GLUT)))

;;TODO: MAKE STUFF DISPLAY IN RIGHT SPOT!

;;for REPL use only!
;(add-classpath "file:/usr/share/java/jogl.jar")
;(add-classpath "file:/usr/share/java/gluegen-rt.jar")

(comment (import
  '(java.awt Frame Dimension)
  '(javax.swing JFrame)
  '(javax.swing.event MouseInputAdapter)
  '(java.awt.event MouseMotionAdapter MouseEvent MouseListener MouseAdapter MouseWheelListener MouseWheelEvent)
  '(javax.media.opengl GLCanvas GLEventListener GL GLAutoDrawable)
  '(javax.media.opengl.glu GLU)
  '(com.sun.opengl.util GLUT)))

(def glu (new GLU))
(def glut (new GLUT))
(def canvas (new GLCanvas))

(defstruct Point :x :y)
(defstruct Line :b :e)

(def last-point (ref nil))

(def lines (ref []))

(defn draw-line [#^GL gl lines] 
  (if (seq lines)
    (let [line (first lines)]
      (if (not (nil? line))
	(let [bx (:x (line :b))
	      by (:y (line :b))
	      ex (:x (line :e))
	      ey (:y (line :e))]
	  (do
;	    (.glVertex3d gl bx by 0.0)
	    (.glVertex3d gl 0.0 0.0 0.0)
;	    (.glVertex3d gl ex ey 0.0)
	    (.glVertex3d gl 0.0 0.0 -5.0)
	    ;(.glVertex3d gl 0.0 0.0 0.0)
	    (draw-line gl (rest lines))))))))

(defn draw-lines [#^GL gl lines]
  (doto gl
    (.glPointSize 4.0)
    ;(.glTranslated 0.0 0.0 -5.0)
    (.glBegin GL/GL_LINES)
    (.glColor3d 1.0 1.0 1.0))
  (draw-line gl lines)
  (doto gl
    (.glPointSize 1.0)
    (.glEnd)))

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
			 (.display canvas))))
       
       (mouseReleased [#^MouseEvent e])))

(def canvas-handle
     (proxy [GLEventListener] []
       
       (init [#^GLAutoDrawable drawable]
	     (let [gl (.getGL drawable)]
	       (doto gl
		 (.glLoadIdentity)
		 ;(.glTranslated 0.0 0.0 -500.0)
		 (.glShadeModel GL/GL_SMOOTH)
		 (.glEnable GL/GL_DEPTH_TEST)
		 (.glEnable GL/GL_POINT_SMOOTH)
		 (.glHint GL/GL_PERSPECTIVE_CORRECTION_HINT GL/GL_NICEST))))
       
       (display [#^GLAutoDrawable drawable]
		(let [gl  (.getGL drawable)]
		  (doto gl
		    (.glClear (bit-or GL/GL_COLOR_BUFFER_BIT GL/GL_DEPTH_BUFFER_BIT))
		    (.glMatrixMode GL/GL_MODELVIEW)
		    (.glLoadIdentity))
		  (draw-lines gl @lines)
		  (.glFlush gl)))
       
       (reshape	  [#^GLAutoDrawable drawable x y w h]
	  (let [gl (.getGL drawable)
		aspect (/ (double w) (double h))]))
       
       (displayChanged [#^GLAutoDrawable drawable m d])))


(defn scribe-window []
  (let [frame (new JFrame "Scribe")]
    (.addMouseMotionListener canvas mouse-handle)
    (.addMouseListener canvas mouse-handle)
    (.addGLEventListener canvas canvas-handle)
    (.setDefaultCloseOperation frame JFrame/DISPOSE_ON_CLOSE)
    (. canvas (setPreferredSize (new Dimension 800 600)))
    (.. frame (getContentPane) (add canvas))
    (doto frame
      (.setSize 800 600)
      (.pack)
      (.setVisible true))))
