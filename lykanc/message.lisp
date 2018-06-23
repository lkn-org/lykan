(cl:in-package :lykanc)

;; ATTRIBUTE_PUPPET
(defun handle-attribute-puppet (message state)
  (jsown:val message "puppet_key"))

;; INSTANCE_DIGEST

;; Unknown message
(defun unknown-message (opcode cmd state)
  state)

;; Puting it together
(defun handle-message (message state)
  (let* ((value (jsown:parse message))
         (opcode (jsown:val value "opcode"))
         (cmd (jsown:val value "message")))
    (cond
      ((string= opcode "ATTRIBUTE_PUPPET") (handle-attribute-puppet cmd state))
      (t (unknown-message opcode cmd state)))))
