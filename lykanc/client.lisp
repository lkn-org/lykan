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
        (cursor-img (make-instance 'fairy:image
                                   :origin (gamekit:vec2 (- (/ *cursor-size* 2))
                                                         (- (/ *cursor-size* 2)))
                                   :key :cursor)))
    (fairy:add-child app (make-instance 'fairy:rectangle
                                        :width (* *viewport-width* *scale*)
                                        :height (* *viewport-height* *scale*)))
    (fairy:add-child ui cursor-layer :with-key :cursor)
    (fairy:add-child cursor-layer cursor-img)
    (fairy:add-child app objects :with-key :game-scene)
    (fairy:add-child app ui :with-key :ui)))

(defmethod init-map ((app client) map-key)
  (let* ((tmx-file (concatenate 'string *maps-dir* map-key ".tmx"))
         (map-layer (make-instance 'fairy/tiled:tile-map :path tmx-file)))
    (setf (fairy:get-child app :game-scene) map-layer))
   (setf (fairy:sort-with (get-objects-layer app))
         (lambda (p1 p2)
           (> (gamekit:y (fairy:origin p1)) (gamekit:y (fairy:origin p2))))))

(defmethod get-objects-layer ((app client))
  (fairy/tiled:get-map-layer (fairy:get-child app :game-scene) "objects"))

(defmethod get-puppet ((app client) key)
  (fairy:get-child (get-objects-layer app) key :test #'equal))

(defun (setf get-puppet) (val app key)
  (setf (fairy:get-child (get-objects-layer app) key :test #'equal)
        val))

(defmethod attribute-puppet ((app client) key)
  (setf (main-puppet app) key))

(defmethod add-puppet ((app client) key x y dir)
  (let ((puppet (new-puppet "character.tsx"
                            x y)))
    (look-at puppet dir)
    (fairy:add-child (get-objects-layer app)
                     puppet
                     :with-key key))
  (when (string= (main-puppet app) key)
    (setf (map-ready? app) t)
    (let ((current-dir (current-direction-angle (keyboard app))))
      (when current-dir ; the user wants to go somewhere
        (wsd:send-text (socket app)
                       (command-set-direction current-dir))
        (wsd:send-text (socket app)
                       "MOVE")))))

(defmethod puppet-starts-moving ((app client) key)
  (starts-moving (get-puppet app key)))

(defmethod puppet-stops-moving ((app client) key)
  (stops-moving (get-puppet app key)))

(defmethod puppet-changes-direction ((app client) key dir)
  (changes-direction (get-puppet app key) dir))

(defmethod puppet-looks-at ((app client) key dir)
  (look-at (get-puppet app key) dir))

(defmethod puppet-hurted ((app client) key)
  (hurted (get-puppet app key)))

(defmethod remove-puppet ((app client) key)
  (fairy:delete-child-with-key (get-objects-layer app)
                               key :test #'equal))

(defmethod puppet-moves ((app client) key x y)
  (setf (fairy:origin (get-puppet app key)) (gamekit:vec2 x y)))

(defmethod puppet-attacks ((app client) puppet-key)
  (starts-attacking (get-puppet app puppet-key)))

(defmethod puppet-stop-attack ((app client) puppet-key)
  (stops-attacking (get-puppet app puppet-key)))

(defmethod get-cursor-x ((app client))
  (gamekit:x (fairy:origin (fairy:get-child (fairy:get-child app :ui) :cursor))))

(defmethod get-cursor-y ((app client))
  (gamekit:y (fairy:origin (fairy:get-child (fairy:get-child app :ui) :cursor))))

(defmethod update-cursor ((app client) dx dy)
  (let* ((dx (* dx *mouse-sensibility*))
         (dy (* dy *mouse-sensibility*))
         (vx (min (max 0 (+ (get-cursor-x app) dx)) *viewport-width*))
         (vy (min (max 0 (+ (get-cursor-y app) dy)) *viewport-height*)))
    (force-cursor app vx vy)))

(defun get-angle (vec)
  (+ (- (atan (gamekit:x vec) (gamekit:y vec))) (/ pi 2)))

(defmethod force-cursor ((app client) x y)
  (let* ((cursor (gamekit:vec2 x y))
         (center (gamekit:vec2 (/ *viewport-width* 2)
                               (/ *viewport-height* 2)))
         (dir-vec (gamekit:subt cursor center))
         (alpha (get-angle dir-vec)))
    (when (map-ready? app)
      (wsd:send-text (socket app)
                     (command-look-at alpha)))
    (setf (fairy:origin (fairy:get-child (fairy:get-child app :ui) :cursor))
          cursor)))

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
            (gamekit:vec2 (round (- dx (* 0.2 (- cursor-x (/ *viewport-width* 2)))))
                          (round (- dy (* 0.2 (- cursor-y (/ *viewport-height* 2))))))))))

(defmethod set-direction ((app client) dir)
  (let ((already-moving? (current-direction (keyboard app))))
    (add-direction (keyboard app) dir)
    (wsd:send-text (socket app)
                   (command-set-direction (current-direction-angle (keyboard app))))
    (when (not already-moving?)
      (wsd:send-text (socket app) "MOVE"))))

(defmethod unset-direction ((app client) dir)
  (remove-direction (keyboard app) dir)
  (let ((dir-angle (current-direction-angle (keyboard app))))
    (if dir-angle
        (wsd:send-text (socket app)
                       (command-set-direction dir-angle))
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

  (gamekit:bind-button
   :mouse-left :pressed
   (lambda ()
     (wsd:send-text (socket app) "ATTACK")))

  (gamekit:bind-button
   :mouse-left :released
   (lambda ()
     (wsd:send-text (socket app) "STOP_ATTACK")))

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
    (setf (last-frame app) new-time))
  (update-camera app))

(defmethod gamekit:draw ((app client))
  (gamekit:with-pushed-canvas ()
    (gamekit:scale-canvas *scale* *scale*)
    (fairy:draw app)))
