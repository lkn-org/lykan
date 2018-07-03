(cl:in-package :lykanc)

(defun command-set-direction (angle)
  (jsown:to-json `(:obj
                   ("opcode" . "SET_DIRECTION")
                   ("arguments" . (:obj
                                   ("angle" . ,angle))))))
