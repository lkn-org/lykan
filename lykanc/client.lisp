(cl:in-package :lykanc)

(gamekit:defgame client (fairy:layer)
  ((last-frame :initform (get-internal-real-time)
               :accessor last-frame)
   (keyboard :initform (make-keyboard)
             :accessor keyboard)
   (socket :initform (wsd:make-client *server-url*)
           :reader socket)
   (cursor-locked :initform t
                  :accessor cursor-locked?)
   (real-cursor-vec :initform (gamekit:vec2 0 0)
                    :accessor real-cursor-vec)
   (map-ready :initform nil
              :accessor map-ready?)
   (main-puppet :initform nil
                :accessor main-puppet))
  (:viewport-width (* *scale* *viewport-width*))
  (:viewport-height (* *scale* *viewport-height*)))

(defmethod init-renderer ((app client))
  (let ((ui (make-instance 'fairy:layer))
        (objects (make-instance 'fairy:layer))
        (cursor-layer (make-instance 'fairy:layer))
        (cursor-img (make-instance 'fairy:rectangle
                                   :origin (gamekit:vec2 (- (/ *cursor-size* 2))
                                                         (- (/ *cursor-size* 2)))
                                   :height *cursor-size*
                                   :width *cursor-size*
                                   :color (gamekit:vec4 0.8 0.3 0.9 1))))
    (fairy:add-child app (make-instance 'fairy:rectangle
                                        :width (* *viewport-width* *scale*)
                                        :height (* *viewport-height* *scale*)))
    (fairy:add-child ui cursor-layer :with-key :cursor)
    (fairy:add-child cursor-layer cursor-img)
    (fairy:add-child app objects :with-key :game-scene)
    (fairy:add-child app ui :with-key :ui)))

(defmethod init-map ((app client) map-key)
  (let* ((tmx-file (concatenate 'string *maps_dir* map-key ".tmx"))
         (map-layer (make-instance 'fairy/tiled:tile-map :path tmx-file)))
    (setf (fairy:get-child app :game-scene) map-layer)))

(defmethod get-objects-layer ((app client))
  (fairy/tiled:get-map-layer (fairy:get-child app :game-scene) "objects"))

(defmethod get-puppet ((app client) key)
  (fairy:get-child (get-objects-layer app) key :test #'equal))

(defun (setf get-puppet) (val app key)
  (setf (fairy:get-child (get-objects-layer app) key :test #'equal)
        val))

(defmethod attribute-puppet ((app client) key)
  (setf (main-puppet app) key))

(defmethod add-puppet ((app client) key x y)
  (let ((puppet (make-instance 'fairy/tiled:tile
                               :current 19
                               :origin (gamekit:vec2 x y)
                               :path "../example/assets/tilesets/character.tsx")))
    (fairy:add-child (get-objects-layer app)
                     puppet
                     :with-key key))
  (when (string= (main-puppet app) key)
    (setf (map-ready? app) t)
    (update-camera app)))

(defmethod puppet-starts-moving ((app client) key)
  (fairy/tiled:start-frame-animation (get-puppet app key)
                                     `(18 19 20)
                                     700))

(defmethod puppet-stops-moving ((app client) key)
  (fairy/tiled:stop-frame-animation (get-puppet app key) 19))

(defmethod remove-puppet ((app client) key)
  (fairy:delete-child-with-key (get-objects-layer app)
                               key :test #'equal))

(defmethod puppet-moves ((app client) key x y)
  (setf (fairy:origin (get-puppet app key)) (gamekit:vec2 x y))
  (when (string= key (main-puppet app))
    (update-camera app)))

(defmethod get-cursor-x ((app client))
  (gamekit:x (fairy:origin (fairy:get-child (fairy:get-child app :ui) :cursor))))

(defmethod get-cursor-y ((app client))
  (gamekit:y (fairy:origin (fairy:get-child (fairy:get-child app :ui) :cursor))))

(defmethod update-cursor ((app client) dx dy)
  (let ((vx (min (max 0 (+ (get-cursor-x app) dx)) *viewport-width*))
        (vy (min (max 0 (+ (get-cursor-y app) dy)) *viewport-height*)))
    (force-cursor app vx vy)))

(defmethod force-cursor ((app client) x y)
  (setf (fairy:origin (fairy:get-child (fairy:get-child app :ui) :cursor))
        (gamekit:vec2 x y))
  (update-camera app))

(defmethod update-camera ((app client))
  (when (map-ready? app)
    (let* ((puppet (get-puppet app (main-puppet app)))
           (cursor-x (get-cursor-x app))
           (cursor-y (get-cursor-y app))
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
      (setf (fairy:origin (fairy:get-child app :game-scene))
            (gamekit:vec2 (round (- dx (* 0.05 (- cursor-x (/ *viewport-width* 2)))))
                          (round (- dy (* 0.05 (- cursor-y (/ *viewport-height* 2))))))))))

(defmethod set-direction ((app client) dir)
  (let ((already-moving? (current-direction (keyboard app))))
    (add-direction (keyboard app) dir)
    (wsd:send-text (socket app) (cond
                                  ((eq dir :up) "UP")
                                  ((eq dir :down) "DOWN")
                                  ((eq dir :right) "RIGHT")
                                  ((eq dir :LEFT) "LEFT")))
    (when (not already-moving?)
      (wsd:send-text (socket app) "MOVE"))))

(defmethod unset-direction ((app client) dir)
  (remove-direction (keyboard app) dir)
  (let ((dir (current-direction (keyboard app))))
    (if dir
        (wsd:send-text (socket app) (cond
                                      ((eq dir :up) "UP")
                                      ((eq dir :down) "DOWN")
                                      ((eq dir :right) "RIGHT")
                                      ((eq dir :LEFT) "LEFT")))
        (wsd:send-text (socket app) "STOP"))))

(defmethod bind-direction ((app client) key dir)
  (gamekit:bind-button key :pressed (lambda () (set-direction app dir)))
  (gamekit:bind-button key :released (lambda () (unset-direction app dir))))

(defmethod gamekit:post-initialize ((app client))
  (init-renderer app)

  (wsd:start-connection (socket app))

  (wsd:on :message (socket app)
          (lambda (message)
            (handle-message message app)))

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
           (force-cursor app (/ *viewport-width* 2) (/ *viewport-height* 2))
           (ge.ng:run (ge.host:for-host () (ge.host:lock-cursor))))
         (ge.ng:run (ge.host:for-host () (ge.host:unlock-cursor))))))

  (bind-direction app :w :up)
  (bind-direction app :a :left)
  (bind-direction app :s :down)
  (bind-direction app :d :right)

  (gamekit:bind-cursor
   (lambda (x y)
     (when (cursor-locked? app)
       (update-cursor app
                      (- x (gamekit:x (real-cursor-vec app)))
                      (- y (gamekit:y (real-cursor-vec app)))))
     (setf (real-cursor-vec app) (gamekit:vec2 x y)))))

(defmethod gamekit:initialize-host ((app client))
  (ge.host:lock-cursor))

(defmethod gamekit:act ((app client))
  (let* ((new-time (get-internal-real-time))
         (dt (/ (* (- new-time (last-frame app)) 1000)
                 internal-time-units-per-second)))
    (fairy:update app dt)
    (setf (last-frame app) new-time)))

(defmethod gamekit:draw ((app client))
  (gamekit:with-pushed-canvas ()
    (gamekit:scale-canvas *scale* *scale*)
    (fairy:draw app)))
