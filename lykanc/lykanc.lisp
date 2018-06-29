(cl:in-package :lykanc)

(gamekit:defgame client ()
  ((last-frame :initform (get-internal-real-time)
               :accessor last-frame)
   (socket :initform (wsd:make-client *server-url*)
           :reader socket)
   (cursor-locked :initform t
                  :accessor cursor-locked?)
   (cursor-vec :initform (gamekit:vec2 0 0)
               :accessor cursor-vec)
   (state :initform (init-state)
          :accessor state))
  (:viewport-width (* *scale* *viewport-width*))
  (:viewport-height (* *scale* *viewport-height*)))

(defmethod gamekit:post-initialize ((app client))
  (wsd:start-connection (socket app))

  (wsd:on :message (socket app)
          (lambda (message)
            (handle-message message (state app))))

  (wsd:on :error (socket app)
          (lambda (error)
            (format t "Got an error: ~S~%" error)))

  (gamekit:bind-button :space :pressed `gamekit:stop)

  (gamekit:bind-button
   :tab :pressed
   (lambda ()
     (setf (cursor-locked? app) (not (cursor-locked? app)))
     (if (cursor-locked? app)
         (progn
           (force-cursor (state app) (/ *viewport-width* 2) (/ *viewport-height* 2))
           (ge.ng:run (ge.host:for-host () (ge.host:lock-cursor))))
         (ge.ng:run (ge.host:for-host () (ge.host:unlock-cursor))))))

  (gamekit:bind-cursor
   (lambda (x y)
     (when (cursor-locked? app)
       (update-cursor (state app) (- x (gamekit:x (cursor-vec app))) (- y (gamekit:y (cursor-vec app)))))
     (setf (cursor-vec app) (gamekit:vec2 x y)))))

(defmethod gamekit:initialize-host ((app client))
  (ge.host:lock-cursor))

(defmethod gamekit:act ((app client))
  (let* ((new-time (get-internal-real-time))
         (dt (/ (* (- new-time (last-frame app)) 1000)
                 internal-time-units-per-second)))
    (fairy:update (state-root (state app)) dt)
    (setf (last-frame app) new-time)))

(defmethod gamekit:draw ((app client))
  (gamekit:with-pushed-canvas ()
    (gamekit:scale-canvas *scale* *scale*)
    (fairy:draw (state-root (state app)))))

(defun run ()
  (gamekit:start 'client))
