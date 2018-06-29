(cl:in-package :lykanc)

;; ATTRIBUTE_PUPPET
(defun handle-attribute-puppet (message state)
  (attribute-puppet state (jsown:val message "puppet_key")))

;; INSTANCE_DIGEST
(defun handle-instance-digest (message state)
  (let ((puppets (jsown:val message "puppets")))
    (jsown:do-json-keys (key puppet-desc) puppets
                        (add-puppet state key
                                    (jsown:val puppet-desc "x")
                                    (jsown:val puppet-desc "y")))))

;; PUPPET ENTERS
(defun handle-puppet-enters (message state)
  (let* ((key (jsown:val message "puppet_key"))
         (digest (jsown:val message "digest")))
    (add-puppet state key (jsown:val digest "x") (jsown:val digest "y"))))

;; PUPPET MOVES
(defun handle-puppet-moves (message state)
  (let* ((key (jsown:val message "puppet_key"))
         (position (jsown:val message "position")))
    (puppet-moves state key (jsown:val position "x") (jsown:val position "y"))))


;; PUPPET LEAVES
(defun handle-puppet-leaves (message state)
  (remove-puppet state (jsown:val message "puppet_key")))

;; PUPPET STARTS
(defun handle-puppet-starts (message state)
  (puppet-starts-moving state (jsown:val message "puppet_key")))

;; PUPPET STOPS
(defun handle-puppet-stops (message state)
  (puppet-stops-moving state (jsown:val message "puppet_key")))

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
      ((string= opcode "PUPPET_STARTS") (handle-puppet-starts cmd state))
      ((string= opcode "PUPPET_MOVES") (handle-puppet-moves cmd state))
      ((string= opcode "PUPPET_STOPS") (handle-puppet-stops cmd state))
      ((string= opcode "PUPPET_LEAVES") (handle-puppet-leaves cmd state))
      (t (unknown-message opcode cmd state)))))
