(uiop:define-package #:lem-rooms-client/connected-hook
  (:use #:cl)
  (:export #:connected-p
           #:disconnect
           #:add
           #:on-connect))
(in-package #:lem-rooms-client/connected-hook)

(defvar *connected-hooks* '())
(defvar *connected* nil)

(defun connected-p () *connected*)

(defun disconnect ()
  (setf *connected* nil))

(defun add (hook)
  (if *connected*
      (funcall hook)
      (push hook *connected-hooks*)))

(defun on-connect ()
  (dolist (hook *connected-hooks*)
    (funcall hook))
  (setf *connected-hooks* '())
  (setf *connected* t))
