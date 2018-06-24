(cl:in-package :lykanc)

(defstruct entity x y width height)

(defstruct state main puppets)

(defun init-state ()
  (make-state :main nil
              :puppets (make-hash-table :test 'equal)))

(defun attribute-puppet (state key)
  (setf (state-main state) key))

(defun add-puppet (state key puppet)
  (setf (gethash key (state-puppets state)) puppet))

(defun remove-puppet (state key)
  (remhash key (state-puppets state)))

(defun puppet-moves (state key x y)
  (setf (entity-x (gethash key (state-puppets state))) x)
  (setf (entity-y (gethash key (state-puppets state))) y))

(defun draw-state (state)
  (maphash (lambda (key value))
           (state-puppets state)))

(defun center-camera-vec (state)
  (let* ((main (state-main state))
         (puppet (gethash main (state-puppets state)))
         (dx (- (/ *viewport-width* 2)
                (entity-x puppet)
                (/ (entity-width puppet) 2)))
         (dy (- (/ *viewport-height* 2)
                (entity-y puppet)
                (/ (entity-height puppet) 2))))
    (gamekit:vec2 dx dy)))
