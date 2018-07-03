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

(defmethod current-direction-angle ((k keyboard))
  (let ((d1 (car (arrows k)))
        (d2 (car (cdr (arrows k)))))
    (cond
      ((and (eq d1 :up) (eq d2 :right)) (/ pi 4))
      ((and (eq d1 :up) (eq d2 :left)) (* 3 (/ pi 4)))
      ((and (eq d1 :up)) (/ pi 2))
      ((and (eq d1 :right) (eq d2 :up)) (/ pi 4))
      ((and (eq d1 :right) (eq d2 :down)) (/ (- pi) 4))
      ((and (eq d1 :right)) 0)
      ((and (eq d1 :down) (eq d2 :right)) (/ (- pi) 4))
      ((and (eq d1 :down) (eq d2 :left)) (* 3 (/ (- pi) 4)))
      ((and (eq d1 :down)) (/ (- pi) 2))
      ((and (eq d1 :left) (eq d2 :down)) (* 3 (/ (- pi) 4)))
      ((and (eq d1 :left) (eq d2 :up)) (* 3 (/ pi 4)))
      ((and (eq d1 :left)) pi)
      (t nil))))
