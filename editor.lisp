(uiop:define-package #:lem-rooms-client/editor
  (:use #:cl
        #:lem)
  (:export #:lem-to-lsp-position
           #:lsp-to-lem-position
           #:position-of
           #:move-to-position*
           #:browser-frontend-p
           #:best-foreground-color
           #:lighten-color
           #:with-save-cursor
           #:close-rightside-window))
(in-package #:lem-rooms-client/editor)

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

(defun lighten-color (color &key (factor 0.5))
  (let ((color (parse-color color)))
    (multiple-value-bind (h s v)
        (lem:rgb-to-hsv (color-red color) (color-green color) (color-blue color))
      (multiple-value-bind (r g b)
          (hsv-to-rgb h
                      (* s (- 1 factor))
                      v)
        (make-color r g b)))))

(defun call-with-save-cursor (buffer function)
  (let* ((point (buffer-point buffer))
         (line (line-number-at-point point))
         (charpos (point-charpos point)))
    (prog1 (funcall function)
      (move-to-line point line)
      (line-offset point 0 charpos))))

(defmacro with-save-cursor (buffer &body body)
  `(call-with-save-cursor ,buffer (lambda () ,@body)))

(defun close-rightside-window ()
  (when (frame-rightside-window (current-frame))
    (delete-rightside-window)))
