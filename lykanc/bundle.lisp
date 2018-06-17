(cl:defpackage :lykanc.bundle
  (:use :cl)
  (:export deliver))

(cl:in-package :lykanc.bundle)

(defun deliver ()
  (gamekit.distribution:deliver :lykanc 'lykanc:app))
