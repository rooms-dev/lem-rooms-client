(uiop:define-package #:lem-rooms-client/user
  (:use #:cl)
  (:export #:convert-user))
(in-package #:lem-rooms-client/user)

(defstruct user
  id
  name
  color)

(defun convert-user (json)
  (make-user :id (gethash "id" json)
             :name (gethash "name" json)
             :color (gethash "color" json)))
