(cl:in-package :lykanc)

;; ATTRIBUTE_PUPPET
(defun handle-attribute-puppet (message state)
  (attribute-puppet state (jsown:val message "puppet_key")))

;; INSTANCE_DIGEST
(defun handle-instance-digest (message state)
  (let ((puppets (jsown:val message "puppets")))
    (jsown:do-json-keys (key puppet-desc) puppets
        (let ((puppet (make-entity :x (jsown:val puppet-desc "x")
                                   :y (jsown:val puppet-desc "y")
                                   :width 24
                                   :height 32)))
          (add-puppet state key puppet)))))

;; PUPPET ENTERS
(defun handle-puppet-enters (message state)
  (let* ((key (jsown:val message "puppet_key"))
         (digest (jsown:val message "digest"))
         (puppet (make-entity :x (jsown:val digest "x")
                              :y (jsown:val digest "y")
                              :width 24
                              :height 32)))
    (add-puppet state key puppet)))

;; PUPPET MOVES
(defun handle-puppet-moves (message state)
  (let* ((key (jsown:val message "puppet_key"))
         (position (jsown:val message "position")))
    (puppet-moves state key (jsown:val position "x") (jsown:val position "y"))))


;; PUPPET_LEAVES
(defun handle-puppet-leaves (message state)
  (remove-puppet state (jsown:val message "puppet_key")))

;; Unknown message
(defun unknown-message (opcode cmd state)
  (print opcode)
  (print cmd))

;; Puting it together
(defun handle-message (message state)
  (let* ((value (jsown:parse message))
         (opcode (jsown:val value "opcode"))
         (cmd (jsown:val value "message")))
    (cond
      ((string= opcode "ATTRIBUTE_PUPPET") (handle-attribute-puppet cmd state))
      ((string= opcode "INSTANCE_DIGEST") (handle-instance-digest cmd state))
      ((string= opcode "PUPPET_ENTERS") (handle-puppet-enters cmd state))
      ((string= opcode "PUPPET_MOVES") (handle-puppet-moves cmd state))
      ((string= opcode "PUPPET_LEAVES") (handle-puppet-leaves cmd state))
      (t (unknown-message opcode cmd state)))))
