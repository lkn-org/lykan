(cl:in-package :lykanc)

(gamekit:defgame app () ()
                 (:viewport-width (* *scale* *viewport-width*))
                 (:viewport-height (* *scale* *viewport-height*)))

(defmethod gamekit:post-initialize ((app app))
  (gamekit:bind-button :space :pressed `gamekit:stop))

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
          (let ((vec (center-camera-vec *state*)))
            (gamekit:translate-canvas (gamekit:x vec) (gamekit:y vec)))
          (maphash (lambda (key val)
                     (gamekit:draw-rect (gamekit:vec2 (entity-x val) (entity-y val))
                                        (entity-width val)
                                        (entity-height val)
                                        :fill-paint (gamekit:vec4 0 0 0 1)))
                   (state-puppets *state*)))
        (print "waiting for a character"))))

(defun run ()
  (gamekit:start 'app))
