(uiop:define-package #:lem-rooms-client/utils
  (:use #:cl)
  (:export #:do-sequence
           #:hash
           #:pretty-json
           #:lem-to-lsp-position
           #:lsp-to-lem-position
           #:position-of
           #:move-to-position*
           #:browser-frontend-p
           #:best-foreground-color))
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

(defun lem-to-lsp-position (position)
  (1- position))

(defun lsp-to-lem-position (position)
  (1+ position))

(defun position-of (point)
  (lem-to-lsp-position (lem:position-at-point point)))

(defun move-to-position* (point position)
  (lem:move-to-position point (lsp-to-lem-position position)))

(defun browser-frontend-p ()
  (and (find-package :lem-server)
       (typep (lem:implementation) (find-symbol "JSONRPC" :lem-server))))

(defun rgb-to-luminance (r g b)
  (let* ((rf (/ r 255.0))
         (gf (/ g 255.0))
         (bf (/ b 255.0)))
    (+ (* 0.2126 rf) (* 0.7152 gf) (* 0.0722 bf))))

(defun best-foreground-color (color)
  (let ((color (lem:parse-color color)))
    (let ((r (lem:color-red color))
          (g (lem:color-green color))
          (b (lem:color-blue color)))
      (let ((luminance (rgb-to-luminance r g b)))
        (if (> luminance 0.5)
            "black"
            "white")))))
