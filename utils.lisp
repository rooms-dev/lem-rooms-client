(uiop:define-package #:lem-rooms-client/utils
  (:use #:cl)
  (:export #:do-sequence
           #:hash
           #:pretty-json
           #:once))
(in-package #:lem-rooms-client/utils)

(defmacro do-sequence ((var sequence) &body body)
  `(map ()
        (lambda (,var) ,@body)
        ,sequence))

(defun hash (&rest plist)
  (let ((hash (make-hash-table :test 'equal)))
    (loop :for (key value) :on plist :by #'cddr
          :do (setf (gethash (change-case:camel-case (string key))
                             hash)
                    value))
    hash))

(defun pretty-json (object)
  (yason:with-output-to-string* (:indent t)
    (yason:encode object)))

(defun %once (name function)
  (unless (get name 'once)
    (funcall function)
    (setf (get name 'once) t)))

(defmacro once (form &key (name (gensym)))
  `(%once ,name (lambda () ,form)))
