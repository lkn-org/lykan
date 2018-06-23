(cl:in-package :lykanc)

(gamekit:defgame app () ()
                 (:fullscreen-p 't))

(defmethod gamekit:post-initialize ((app app))
  (gamekit:bind-button :mouse-left :released `gamekit:stop))

(defvar *client* (wsd:make-client "ws://localhost:4000"))

(defvar *state* nil)

(wsd:on :message *client*
        (lambda (message)
          (setf *state* (handle-message message *state*))))

(wsd:on :error *client*
        (lambda (error)
          (format t "Got an error: ~S~%" error)))

(handler-case (wsd:start-connection *client*)
  (usocket:connection-refused-error nil (print "was not able to connect")))

(defun run ()
  (gamekit:start 'app))
