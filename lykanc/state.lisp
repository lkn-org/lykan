(cl:in-package :lykanc)

(defstruct entity x y width height)

(defstruct state puppets)

(defun init-state ()
  (make-state :puppets (make-hash-table :test 'equal)))

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
