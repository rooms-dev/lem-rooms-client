(uiop:define-package #:lem-rooms-client/config
  (:use #:cl
        #:lem)
  (:export #:*rooms-url*
           #:*editor-server-url*
           #:access-token
           #:user
           #:user-name))
(in-package #:lem-rooms-client/config)

(defparameter *rooms-url* "http://163.44.114.56:8080")

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

(let ((cache nil))
  (defun user-name ()
    (or cache
        (setf cache
              (getf (user) :github-login)))))
