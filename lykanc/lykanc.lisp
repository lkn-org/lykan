(cl:in-package :lykanc)

(gamekit:defgame app () ()
                 (:viewport-width (* *scale* *viewport-width*))
                 (:viewport-height (* *scale* *viewport-height*)))

(defvar *x* 0)
(defvar *y* 0)
(defvar *vx* 0)
(defvar *vy* 0)
(defvar *cursor-size* 10)
(defvar *locked* t)

(defun init-cursor-values ()
  (let ((init-x (/ *viewport-width* 2))
        (init-y (/ *viewport-height* 2)))
    (setf *vx* init-x)
    (setf *vy* init-y)))

(init-cursor-values)

(defmethod gamekit:post-initialize ((app app))
  (gamekit:bind-button :space :pressed `gamekit:stop)
  (gamekit:bind-button
   :tab :pressed
   (lambda ()
     (setf *locked* (not *locked*))
     (if *locked*
         (progn
           (init-cursor-values)
           (ge.ng:run (ge.host:for-host () (ge.host:lock-cursor))))
         (ge.ng:run (ge.host:for-host () (ge.host:unlock-cursor))))))
  (gamekit:bind-cursor
   (lambda (x y)
     (if *locked*
         (progn
           (setf *vx* (min (max 0 (+ *vx* (- x *x*))) *viewport-width*))
           (setf *vy* (min (max 0 (+ *vy* (- y *y*))) *viewport-height*))))
     (setf *x* x)
     (setf *y* y))))

(defmethod gamekit:initialize-host ((app app))
  (ge.host:lock-cursor))

(defvar *client* (wsd:make-client "ws://localhost:4000"))

(defvar *state* (init-state))

(wsd:on :message *client*
        (lambda (message)
          (handle-message message *state*)
          (print *state*)))

(wsd:on :error *client*
        (lambda (error)
          (format t "Got an error: ~S~%" error)))

(handler-case (wsd:start-connection *client*)
  (usocket:connection-refused-error nil (print "was not able to connect")))

(defmethod gamekit:draw ((app app))
  (gamekit:with-pushed-canvas ()
    (gamekit:scale-canvas *scale* *scale*)
    (if (state-main *state*)
        (progn
          (gamekit:with-pushed-canvas ()
            (let ((vec (center-camera-vec *state* *vx* *vy*)))
              (gamekit:translate-canvas (gamekit:x vec) (gamekit:y vec)))
            (maphash (lambda (key val)
                       (gamekit:draw-rect (gamekit:vec2 (entity-x val) (entity-y val))
                                          (entity-width val)
                                          (entity-height val)
                                          :fill-paint (gamekit:vec4 0 0 0 1)))
                     (state-puppets *state*))))
        (print "waiting for a character"))
    (gamekit:draw-rect (gamekit:vec2 (- *vx* (/ *cursor-size* 2))
                                     (- *vy* (/ *cursor-size* 2)))
                       *cursor-size*
                       *cursor-size*
                       :fill-paint (gamekit:vec4 0.9 0.3 0.7 1))))

(defun run ()
  (gamekit:start 'app))
