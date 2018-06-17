(cl:in-package :lykanc)

(gamekit:defgame app () ()
                 (:fullscreen-p 't))

(defmethod gamekit:post-initialize ((app app))
  (gamekit:bind-button :mouse-left :released `gamekit:stop))

(defun run ()
  (gamekit:start 'app))
