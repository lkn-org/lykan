(cl:in-package :lykanc)

;; ATTRIBUTE_PUPPET
(defun handle-attribute-puppet (message app)
  (attribute-puppet app (jsown:val message "puppet_key")))

;; INSTANCE_DIGEST
(defun handle-instance-digest (message app)
  (let ((puppets (jsown:val message "puppets")))
    (jsown:do-json-keys (key puppet-desc) puppets
                        (add-puppet app key
                                    (jsown:val puppet-desc "x")
                                    (jsown:val puppet-desc "y")))))

;; PUPPET ENTERS
(defun handle-puppet-enters (message app)
  (let* ((key (jsown:val message "puppet_key"))
         (digest (jsown:val message "digest")))
    (add-puppet app key (jsown:val digest "x") (jsown:val digest "y"))))

;; PUPPET MOVES
(defun handle-puppet-moves (message app)
  (let* ((key (jsown:val message "puppet_key"))
         (position (jsown:val message "position")))
    (puppet-moves app key (jsown:val position "x") (jsown:val position "y"))))


;; PUPPET LEAVES
(defun handle-puppet-leaves (message app)
  (remove-puppet app (jsown:val message "puppet_key")))

;; PUPPET STARTS
(defun handle-puppet-starts (message app)
  (puppet-starts-moving app (jsown:val message "puppet_key")))

;; PUPPET STOPS
(defun handle-puppet-stops (message app)
  (puppet-stops-moving app (jsown:val message "puppet_key")))

;; Unknown message
(defun unknown-message (opcode cmd app)
  (print opcode)
  (print cmd))

;; Puting it together
(defun handle-message (message app)
  (let* ((value (jsown:parse message))
         (opcode (jsown:val value "opcode"))
         (cmd (jsown:val value "message")))
    (cond
      ((string= opcode "ATTRIBUTE_PUPPET") (handle-attribute-puppet cmd app))
      ((string= opcode "INSTANCE_DIGEST") (handle-instance-digest cmd app))
      ((string= opcode "PUPPET_ENTERS") (handle-puppet-enters cmd app))
      ((string= opcode "PUPPET_STARTS") (handle-puppet-starts cmd app))
      ((string= opcode "PUPPET_MOVES") (handle-puppet-moves cmd app))
      ((string= opcode "PUPPET_STOPS") (handle-puppet-stops cmd app))
      ((string= opcode "PUPPET_LEAVES") (handle-puppet-leaves cmd app))
      (t (unknown-message opcode cmd app)))))
