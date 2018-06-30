(cl:in-package :lykanc)

(defclass keyboard ()
  ((arrows :initform nil
          :accessor arrows)))

(defun make-keyboard ()
  (make-instance 'keyboard))

(defmethod add-direction ((k keyboard) dir)
  (when (not (member dir (arrows k)))
    (push dir (arrows k))))

(defmethod remove-direction ((k keyboard) dir)
  (setf (arrows k) (remove dir (arrows k))))

(defmethod current-direction ((k keyboard))
  (car (arrows k)))
