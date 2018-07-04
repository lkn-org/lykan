(cl:in-package :lykanc)

(defmacro define-tileset (str)
  `(gamekit:define-image (alexandria:make-keyword ,str) ,str))

(gamekit:register-resource-package :keyword *tilesets-dir*)

(define-tileset "character.png")
(define-tileset "map.png")

(gamekit:define-image :cursor "cursor.png")

(gamekit:define-font :lykanc-font "NotoSansMono-Regular.ttf")

(defun tileset-path (name)
  (concatenate 'string *tilesets-dir* "/" name))
