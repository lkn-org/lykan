(cl:in-package :lykanc)

(defmacro push-action (&rest body)
  `(gamekit:push-action (lambda ()
                          ,@body)))

;; ATTRIBUTE_PUPPET
(defun handle-attribute-puppet (message app)
  (push-action (attribute-puppet app (jsown:val message "puppet_key"))))

;; INSTANCE_DIGEST
(defun handle-instance-digest (message app)
  (push-action
   (let ((map-key (jsown:val (jsown:val message "map") "map_key"))
         (puppets (jsown:val message "puppets")))
     (init-map app map-key)
     (jsown:do-json-keys (key puppet-desc) puppets
                         (add-puppet app key
                                     (jsown:val puppet-desc "x")
                                     (jsown:val puppet-desc "y"))))))

;; PUPPET ENTERS
(defun handle-puppet-enters (message app)
  (push-action
   (let* ((key (jsown:val message "puppet_key"))
          (digest (jsown:val message "digest")))
     (add-puppet app key (jsown:val digest "x") (jsown:val digest "y")))))

;; PUPPET MOVES
(defun handle-puppet-moves (message app)
  (push-action
   (let* ((key (jsown:val message "puppet_key"))
          (position (jsown:val message "position")))
     (puppet-moves app key (jsown:val position "x") (jsown:val position "y")))))


;; PUPPET LEAVES
(defun handle-puppet-leaves (message app)
  (push-action (remove-puppet app (jsown:val message "puppet_key"))))

;; PUPPET STARTS
(defun handle-puppet-starts (message app)
  (push-action (puppet-starts-moving app (jsown:val message "puppet_key"))))

;; PUPPET STOPS
(defun handle-puppet-stops (message app)
  (push-action (puppet-stops-moving app (jsown:val message "puppet_key"))))

;; PUPPET DIRECTION
(defun handle-puppet-direction (message app)
  (push-action
   (let* ((str-dir (jsown:val message "direction"))
          (dir (cond
                 ((string= str-dir "down") :down)
                 ((string= str-dir "up") :up)
                 ((string= str-dir "right") :right)
                 ((string= str-dir "left") :left))))
     (puppet-changes-direction app
                               (jsown:val message "puppet_key")
                               dir))))

;; PUPPET STARTS ATTACKING
(defun handle-puppet-attacks (message app)
  (push-action (puppet-attacks app (jsown:val message "puppet_key"))))

;; PUPPET STOPS ATTACKING
(defun handle-puppet-stops-attack (message app)
  (push-action (puppet-stop-attack app (jsown:val message "puppet_key"))))

;; PUPPET HURTED
(defun handle-puppet-hurted (message app)
  (push-action (puppet-hurted app (jsown:val message "puppet_key"))))

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
      ((string= opcode "PUPPET_DIRECTION") (handle-puppet-direction cmd app))
      ((string= opcode "PUPPET_STARTS_ATTACKING") (handle-puppet-attacks cmd app))
      ((string= opcode "PUPPET_STOPS_ATTACKING") (handle-puppet-stops-attack cmd app))
      ((string= opcode "PUPPET_HURTED") (handle-puppet-hurted cmd app))
      (t (unknown-message opcode cmd app)))))
