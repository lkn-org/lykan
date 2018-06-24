(cl:in-package :lykanc)

(defstruct entity x y width height)

(defstruct state main root cursor-pos objects ui cursor-img puppets)

(defun init-state ()
  (let ((root (fairy:init-root))
        (ui (fairy:new-layer 0 0))
        (objects (fairy:new-layer 0 0))
        (cursor-img (fairy:new-rectangle (/ *cursor-size* 2)
                                         (/ *cursor-size* 2)
                                         *cursor-size*
                                         *cursor-size*
                                         (gamekit:vec4 0.8 0.3 0.9 1))))
    (fairy:layer-add-child ui cursor-img)
    (fairy:root-add-child root ui)
    (fairy:root-add-child root objects)

    (make-state :main nil
                :root root
                :cursor-pos (gamekit:vec2 0 0)
                :objects objects
                :ui (fairy:new-layer 0 0)
                :cursor-img cursor-img
                :puppets (make-hash-table :test 'equal))))

(defun attribute-puppet (state key)
  (setf (state-main state) key))

(defun add-puppet (state key x y)
  (let ((puppet (fairy:new-rectangle x y 24 32 (gamekit:vec4 0 0 0 1))))
    (fairy:layer-add-child (state-objects state) puppet)
    (setf (gethash key (state-puppets state)) puppet))
  (if (string= (state-main state) key)
      (update-camera state)))

(defun remove-puppet (state key)
  (fairy:layer-delete-child (state-objects state) (gethash key (state-puppets state)))
  (remhash key (state-puppets state)))

(defun puppet-moves (state key x y)
  (setf (fairy:rectangle-origin (gethash key (state-puppets state)))
        (gamekit:vec2 x y)))

(defun get-cursor-x (state)
  (gamekit:x (state-cursor-pos state)))

(defun get-cursor-y (state)
  (gamekit:y (state-cursor-pos state)))

(defun update-cursor (state x y ox oy)
  (let ((vx (min (max 0 (+ (gamekit:x (state-cursor-pos state))
                           (- x ox)))
                 *viewport-width*))
        (vy (min (max 0 (+ (gamekit:y (state-cursor-pos state))
                           (- y oy)))
                 *viewport-height*)))
    (force-cursor state vx vy)))

(defun force-cursor (state x y)
  (setf (fairy:rectangle-origin (state-cursor-img state))
        (gamekit:vec2 (- x (/ *cursor-size* 2))
                      (- y (/ *cursor-size* 2))))
  (setf (state-cursor-pos state)
        (gamekit:vec2 x y))
  (update-camera state))

(defun update-camera (state)
  (let* ((cursor-x (gamekit:x (state-cursor-pos state)))
         (cursor-y (gamekit:y (state-cursor-pos state)))
         (puppet (gethash (state-main state) (state-puppets state)))
         (puppet-x (gamekit:x (fairy:rectangle-origin puppet)))
         (puppet-y (gamekit:y (fairy:rectangle-origin puppet)))
         (puppet-width (fairy:rectangle-width puppet))
         (puppet-height (fairy:rectangle-height puppet))
         (dx (- (/ *viewport-width* 2)
                puppet-x
                (/ puppet-width 2)))
         (dy (- (/ *viewport-height* 2)
                puppet-y
                (/ puppet-height 2))))
    (setf (fairy:layer-origin (state-objects state))
          (gamekit:vec2 (- dx (* 0.2 (- cursor-x (/ *viewport-width* 2))))
                        (- dy (* 0.2 (- cursor-y (/ *viewport-height* 2))))))))
