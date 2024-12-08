(defpackage :lem-rooms-client/management-buffer
  (:use #:cl
        #:lem
        #:lem-rooms-client/editor
        #:lem-rooms-client/utils)
  (:local-nicknames (#:api-client #:lem-rooms-client/api-client))
  (:export #:update))
(in-package :lem-rooms-client/management-buffer)

(defun update (buffer &key users client)
  (erase-buffer buffer)
  (with-point ((point (buffer-point buffer) :left-inserting))
    (insert-string point
                   "Rooms"
                   :attribute (make-attribute
                               :bold t
                               :foreground (best-foreground-color (background-color))))
    (insert-character point #\newline)
    (insert-character point #\newline)
    (insert-string point (format nil "Status:~%"))
    (ecase (api-client:client-connection-status client)
      (:connecting
       (insert-string point "Connecting..." :attribute (make-attribute :foreground "orange")))
      (:connected
       (insert-string point "Connected" :attribute (make-attribute :foreground "green")))
      (:disconnected
       (insert-string point "Disconnected" :attribute (make-attribute :foreground "red"))))
    (insert-character point #\newline)
    (insert-string point (format nil "~%Users:~%"))
    (do-sequence (user users)
      (let ((name (gethash "name" user))
            (color (gethash "color" user)))
        (insert-string point
                       (format nil " ~A " name)
                       :attribute (make-attribute :foreground (best-foreground-color color)
                                                  :background color))
        (insert-character point #\newline)))
    (insert-character point #\newline)
    (insert-character point #\newline)))
