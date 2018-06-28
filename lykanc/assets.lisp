(cl:in-package :lykanc)

(defmacro define-tileset (str)
  `(gamekit:define-image (alexandria:make-keyword ,str) ,str))

(gamekit:register-resource-package :keyword "../example/assets/tilesets/")
(define-tileset "character.png")
