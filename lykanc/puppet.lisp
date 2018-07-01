(cl:in-package :lykanc)

(defclass puppet (fairy:layer)
  ((direction :initform :down
              :accessor direction)
   (action :initform :waiting
           :accessor action)))

(defun new-puppet (path-tileset x y)
  (let ((layer (make-instance 'puppet
                             :origin (gamekit:vec2 x y)))
        (tile (make-instance 'fairy/tiled:tile
                             :current 19
                             :path path-tileset)))
    (fairy:add-child layer tile :with-key :character)
    layer))

(defun moving-frames (dir)
  (cond
    ((eq dir :down) `(19 20 18))
    ((eq dir :up) `(1 2 0))
    ((eq dir :right) `(10 11 9))
    ((eq dir :left) `(28 29 27))))

(defun waiting-frame (dir)
  (cond
    ((eq dir :down) 19)
    ((eq dir :up) 1)
    ((eq dir :right) 10)
    ((eq dir :left) 28)))

(defmethod starts-moving ((p puppet))
  (setf (action p) :moving)
  (fairy/tiled:start-frame-animation (fairy:get-child p :character)
                                     (moving-frames (direction p))
                                     600))

(defmethod stops-moving ((p puppet))
  (setf (action p) :waiting)
  (fairy/tiled:stop-frame-animation (fairy:get-child p :character)
                                    (waiting-frame (direction p))))

(defmethod fairy:width ((p puppet))
  (fairy:width (fairy:get-child p :character)))

(defmethod fairy:height ((p puppet))
  (fairy:height (fairy:get-child p :character)))

(defmethod changes-direction ((p puppet) dir)
  (setf (direction p) dir)
  (cond
    ((eq (action p) :moving)  (starts-moving p))
    ((eq (action p) :waiting) (stops-moving p))))
