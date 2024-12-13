(defpackage :lem-rooms-client/management-pane
  (:use #:cl
        #:lem
        #:lem-rooms-client/editor
        #:lem-rooms-client/utils)
  (:local-nicknames (#:api-client #:lem-rooms-client/api-client))
  (:export #:make-management-pane
           #:update))
(in-package :lem-rooms-client/management-pane)

(defstruct comment
  user-name
  user-color
  text)

(defclass management-pane ()
  ((buffer :initarg :buffer
           :reader management-pane-buffer)
   (users-buffer :initarg :users-buffer
                 :reader management-pane-users-buffer)
   (comment-buffer :initarg :comment-buffer
                   :reader management-pane-comment-buffer)))

(defun make-management-pane ()
  (let ((buffer (make-buffer "*Rooms right-side-pane*" :temporary t :enable-undo-p nil)))
    (setf (not-switchable-buffer-p buffer) t)
    (make-instance 'management-pane
                   :buffer buffer
                   :users-buffer (make-buffer "*Rooms users*" :temporary t :enable-undo-p nil)
                   :comment-buffer (make-buffer "*Rooms comments*" :temporary t :enable-undo-p nil))))

(defun insert-color-text (point string color)
  (insert-string point
                 string
                 :attribute (make-attribute :foreground (best-foreground-color color)
                                            :background color)))

(defun update (pane &key client (users nil users-p) adding-comments)
  (let ((buffer (management-pane-buffer pane)))
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
      (let ((users-buffer (management-pane-users-buffer pane)))
        (when users-p
          (erase-buffer users-buffer)
          (with-point ((point (buffer-point users-buffer) :left-inserting))
            (do-sequence (user users)
              (let ((name (gethash "name" user))
                    (color (gethash "color" user)))
                (insert-color-text point (format nil " ~A " name) color)
                (insert-character point #\newline)))))
        (insert-buffer point users-buffer))
      (insert-character point #\newline)
      (insert-character point #\newline)
      (let ((comment-buffer (management-pane-comment-buffer pane)))
        (with-point ((point (buffer-point comment-buffer) :left-inserting))
          (dolist (comment adding-comments)
            (insert-color-text point
                               (comment-user-name comment)
                               (comment-user-color comment))))
        (insert-buffer point comment-buffer)))))
