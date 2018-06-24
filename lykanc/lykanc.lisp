(cl:in-package :lykanc)

(defvar *state* (init-state))

(gamekit:defgame app () ()
                 (:viewport-width (* *scale* *viewport-width*))
                 (:viewport-height (* *scale* *viewport-height*)))

; the websocket
(defvar *client* (wsd:make-client "ws://localhost:4000"))
(wsd:on :message *client*
        (lambda (message)
          (handle-message message *state*)))

(wsd:on :error *client*
        (lambda (error)
          (format t "Got an error: ~S~%" error)))

(defvar *locked* t)
(defvar *x* 0)
(defvar *y* 0)

(defun init-cursor-values (state)
  (let* ((init-x (/ *viewport-width* 2))
         (init-y (/ *viewport-height* 2)))
    (force-cursor *state* init-x init-y)))

(defmethod gamekit:post-initialize ((app app))
  (gamekit:bind-button :space :pressed `gamekit:stop)

  (gamekit:bind-button
   :tab :pressed
   (lambda ()
     (setf *locked* (not *locked*))
     (if *locked*
         (progn
           (force-cursor *state*
                         (/ *viewport-width* 2) (/ *viewport-height* 2))
           (ge.ng:run (ge.host:for-host () (ge.host:lock-cursor))))
         (ge.ng:run (ge.host:for-host () (ge.host:unlock-cursor))))))

  (gamekit:bind-cursor
   (lambda (x y)
     (if *locked*
         (update-cursor *state* x y *x* *y*))
     (setf *x* x)
     (setf *y* y))))

(defmethod gamekit:initialize-host ((app app))
  (ge.host:lock-cursor))

(handler-case (wsd:start-connection *client*)
  (usocket:connection-refused-error nil (print "was not able to connect")))

(defmethod gamekit:draw ((app app))
  (gamekit:with-pushed-canvas ()
    (gamekit:scale-canvas *scale* *scale*)
    (fairy:draw (state-root *state*))))

(defun run ()
  (gamekit:start 'app))
