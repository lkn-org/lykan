(cl:in-package :lykanc)

(defclass puppet (fairy:layer)
  ((direction :initform :down
              :accessor direction)
   (moving :initform nil
           :accessor moving?)
   (attacking :initform nil
              :accessor attacking?)))

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

(defun attacking-frames (dir)
  (cond
    ((eq dir :down) `(22 23 21))
    ((eq dir :up) `(4 5 3))
    ((eq dir :right) `(13 14 12))
    ((eq dir :left) `(31 32 30))))

(defun waiting-frame (dir)
  (cond
    ((eq dir :down) 19)
    ((eq dir :up) 1)
    ((eq dir :right) 10)
    ((eq dir :left) 28)))

(defmethod update-animation ((p puppet))
  (cond
    ((attacking? p)
     (fairy/tiled:start-frame-animation (fairy:get-child p :character)
                                        (attacking-frames (direction p))
                                        600))
    ((moving? p)
     (fairy/tiled:start-frame-animation (fairy:get-child p :character)
                                        (moving-frames (direction p))
                                        600))
    (t
     (fairy/tiled:stop-frame-animation (fairy:get-child p :character)
                                         (waiting-frame (direction p))))))

(defmethod starts-moving ((p puppet))
  (setf (moving? p) t)
  (update-animation p))

(defmethod hurted ((p puppet))
  (let* ((h (fairy:height p))
         (w (fairy:width p))
         (damages (make-instance 'fairy:text
                                 :value "Hit!"
                                 :font (gamekit:make-font :lykanc-font 10)
                                 :origin (gamekit:vec2 3 (- h 5)))))
    (fairy:add-child p damages)
    (fairy:goto damages (gamekit:vec2 3 (+ h 15)) 700
                :then (lambda ()
                        (fairy:delete-child p damages)))))

(defmethod stops-moving ((p puppet))
  (setf (moving? p) nil)
  (update-animation p))

(defmethod starts-attacking ((p puppet))
  (setf (attacking? p) t)
  (update-animation p))

(defmethod stops-attacking ((p puppet))
  (setf (attacking? p) nil)
  (update-animation p))

(defmethod fairy:width ((p puppet))
  (fairy:width (fairy:get-child p :character)))

(defmethod fairy:height ((p puppet))
  (fairy:height (fairy:get-child p :character)))

(defmethod changes-direction ((p puppet) dir)
  (when (not (eq (direction p) dir))
    (setf (direction p) dir)
    (update-animation p)))

(defun get-dir (angle)
  (cond
    ((<= angle (- (* 3 (/ pi 4)))) :down)
    ((<= angle (- (* 1 (/ pi 4)))) :left)
    ((<= angle (* 1 (/ pi 4))) :up)
    ((<= angle (* 3 (/ pi 4))) :right)
    (t :down)))

(defmethod look-at ((p puppet) angle)
  (changes-direction p (get-dir angle)))
