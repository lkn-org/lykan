(cl:in-package :lykanc)

(defun command-set-direction (angle)
  (jsown:to-json `(:obj
                   ("opcode" . "SET_DIRECTION")
                   ("arguments" . (:obj
                                   ("angle" . ,angle))))))

(defun command-look-at (angle)
  (jsown:to-json `(:obj
                   ("opcode" . "LOOK_AT")
                   ("arguments" . (:obj
                                   ("angle" . ,angle))))))
