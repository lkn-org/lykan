(asdf:defsystem lykanc
  :description "Lykan Game Client"
  :author "lthms"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit
               websocket-driver-client
               jsown)
  :components ((:file "project")
               (:file "state")
               (:file "message")
               (:file "lykanc")))

(asdf:defsystem lykanc/bundle
  :description "Bundle the Lykan Game Client"
  :author "lthms"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit/distribution lykanc)
  :components ((:file "bundle")))
