(cl:in-package :lykanc)

(defun resource-key (path)
  (alexandria:make-keyword (file-namestring path)))

(defstruct state main root cursor-pos cursor)

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
    (fairy:add-child ui cursor-layer :with-key :cursor)
    (fairy:add-child cursor-layer cursor-img)
    (fairy:add-child root ui :with-key :ui)
    (fairy:add-child root objects :with-key :objects)

    (make-state :main nil
                :root root
                :cursor cursor-layer)))

(defun get-puppet (state key)
  (fairy:get-child (fairy:get-child (state-root state) :objects)
                   key :test #'equal))

(defun (setf get-puppet) (val state key)
  (setf (fairy:get-child (fairy:get-child (state-root state) :objects)
                         key
                         :test #'equal)
        val))

(defun attribute-puppet (state key)
  (setf (state-main state) key))

(defun add-puppet (state key x y)
  (let ((puppet (make-instance 'fairy/tiled:tile
                               :current 19
                               :origin (gamekit:vec2 x y)
                               :get-resource #'resource-key
                               :path "../example/assets/tilesets/character.tsx")))
    (fairy:add-child (fairy:get-child (state-root state) :objects)
                     puppet
                     :with-key key))
  (if (string= (state-main state) key)
      (update-camera state)))

(defun puppet-starts-moving (state key)
  (fairy/tiled:start-frame-animation (get-puppet state key)
                                     `(18 19 20)
                                     700))

(defun puppet-stops-moving (state key)
  (fairy/tiled:stop-frame-animation (get-puppet state key) 19))

(defun remove-puppet (state key)
  (fairy:delete-child-with-key (fairy:get-child (state-root state) :objects)
                               key :test #'equal))

(defun puppet-moves (state key x y)
  (setf (fairy:origin (get-puppet state key)) (gamekit:vec2 x y)))

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
  (when (state-main state)
    (let* ((cursor-x (get-cursor-x state))
           (cursor-y (get-cursor-y state))
           (puppet (get-puppet state (state-main state)))
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
      (setf (fairy:origin (fairy:get-child (state-root state) :objects))
            (gamekit:vec2 (- dx (* 0.2 (- cursor-x (/ *viewport-width* 2))))
                          (- dy (* 0.2 (- cursor-y (/ *viewport-height* 2)))))))))
