(uiop:define-package #:lem-rooms-client/config
  (:use #:cl
        #:lem)
  (:export #:rooms-url
           #:access-token
           #:user))
(in-package #:lem-rooms-client/config)

(defun rooms-url ()
  (uiop:getenv "ROOMS_URL"))

(defun access-token ()
  (config :rooms.access-token))

(defun (setf access-token) (access-token)
  (setf (config :rooms.access-token) access-token))

(defun user ()
  (config :rooms.user))

(defun (setf user) (plist)
  (assert (getf plist :id))
  (assert (getf plist :github-login))
  (assert (getf plist :avatar-url))
  (setf (config :rooms.user) plist))
