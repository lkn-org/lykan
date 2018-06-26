(cl:in-package :lykanc)

(defstruct entity x y width height)

(defstruct state main root cursor-pos objects ui cursor puppets)

(defun init-state ()
  (let ((root (make-instance 'fairy:layer))
        (ui (make-instance 'fairy:layer))
        (objects (make-instance 'fairy:layer))
        (cursor-layer (make-instance 'fairy:layer))
        (cursor-img (make-instance 'fairy:rectangle
                                   :origin (gamekit:vec2 (- (/ *cursor-size* 2))
                                                         (- (/ *cursor-size* 2)))
                                   :height *cursor-size*
                                   :width *cursor-size*
                                   :color (gamekit:vec4 0.8 0.3 0.9 1))))
    (fairy:add-child ui cursor-layer)
    (fairy:add-child cursor-layer cursor-img)
    (fairy:add-child root ui)
    (fairy:add-child root objects)

    (make-state :main nil
                :root root
                :objects objects
                :ui ui
                :cursor cursor-layer
                :puppets (make-hash-table :test 'equal))))

(defun attribute-puppet (state key)
  (setf (state-main state) key))

(defun add-puppet (state key x y)
  (let ((puppet (make-instance 'fairy:rectangle
                               :origin (gamekit:vec2 x y)
                               :width 24
                               :height 32)))
    (fairy:add-child (state-objects state) puppet)
    (setf (gethash key (state-puppets state)) puppet))
  (if (string= (state-main state) key)
      (update-camera state)))

(defun remove-puppet (state key)
  (fairy:delete-child (state-objects state) (gethash key (state-puppets state)))
  (remhash key (state-puppets state)))

(defun puppet-moves (state key x y)
  (setf (fairy:origin (gethash key (state-puppets state)))
        (gamekit:vec2 x y)))

(defun get-cursor-x (state)
  (gamekit:x (fairy:origin (state-cursor state))))

(defun get-cursor-y (state)
  (gamekit:y (fairy:origin (state-cursor state))))

(defun update-cursor (state dx dy)
  (let ((vx (min (max 0 (+ (get-cursor-x state) dx)) *viewport-width*))
        (vy (min (max 0 (+ (get-cursor-y state) dy)) *viewport-height*)))
    (force-cursor state vx vy)))

(defun force-cursor (state x y)
  (setf (fairy:origin (state-cursor state))
        (gamekit:vec2 x y))
  (update-camera state))

(defun update-camera (state)
  (let* ((cursor-x (get-cursor-x state))
         (cursor-y (get-cursor-y state))
         (puppet (gethash (state-main state) (state-puppets state)))
         (puppet-x (gamekit:x (fairy:origin puppet)))
         (puppet-y (gamekit:y (fairy:origin puppet)))
         (puppet-width (fairy:width puppet))
         (puppet-height (fairy:height puppet))
         (dx (- (/ *viewport-width* 2)
                puppet-x
                (/ puppet-width 2)))
         (dy (- (/ *viewport-height* 2)
                puppet-y
                (/ puppet-height 2))))
    (setf (fairy:origin (state-objects state))
          (gamekit:vec2 (- dx (* 0.2 (- cursor-x (/ *viewport-width* 2))))
                        (- dy (* 0.2 (- cursor-y (/ *viewport-height* 2))))))))
