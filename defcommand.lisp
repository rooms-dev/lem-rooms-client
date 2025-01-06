(uiop:define-package #:lem-rooms-client/defcommand
  (:use #:cl)
  (:export #:define-rooms-command
           #:rooms-command-name
           #:rooms-command-description
           #:list-rooms-commands))
(in-package #:lem-rooms-client/defcommand)

(defclass rooms-command-advice () ())

(defmacro define-rooms-command (name params (&rest arg-descriptors) &body body)
  `(progn
     (lem:define-command (,name (:advice-classes rooms-command-advice)) ,params (,@arg-descriptors)
       ,@body)))

(defstruct rooms-command
  name
  description)

(defun list-rooms-commands ()
  (loop :for class :in (reverse (c2mop:class-direct-subclasses (find-class 'rooms-command-advice)))
        :collect (make-rooms-command :name (string-downcase (class-name class))
                                     :description (documentation (class-name class) 'function))))
