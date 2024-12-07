(uiop:define-package #:lem-rooms-client/config
  (:use #:cl
        #:lem)
  (:export #:rooms-url))
(in-package #:lem-rooms-client/config)

(defun rooms-url ()
  (uiop:getenv "ROOMS_URL"))
