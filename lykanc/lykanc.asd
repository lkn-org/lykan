(asdf:defsystem lykanc
  :description "Lykan Game Client"
  :author "lthms"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit)
  :components ((:file "project")
               (:file "lykanc")))

(asdf:defsystem lykanc/bundle
  :description "Bundle the Lykan Game Client"
  :author "lthms"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (trivial-gamekit/distribution lykanc)
  :components ((:file "bundle")))
