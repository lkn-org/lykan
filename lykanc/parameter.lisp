(cl:in-package :lykanc)

(defparameter *viewport-width* 320)
(defparameter *viewport-height* 240)
(defparameter *scale* 2)
(defparameter *cursor-size* 10)
(defparameter *mouse-sensibility* 0.5)
(defparameter *server-url* "ws://localhost:4000")
(defparameter *lykan-assets-dir* (uiop:getenv "LYKAN_ASSETS_DIR"))

(defparameter *tilesets-dir* (concatenate 'string *lykan-assets-dir* "/tilesets/"))
(defparameter *maps-dir* (concatenate 'string *lykan-assets-dir* "/maps/"))
