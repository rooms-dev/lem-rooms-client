(uiop:define-package #:lem-rooms-client/buffer
  (:use #:cl
        #:lem)
  (:export #:room-id
           #:path
           #:find-buffer-by-room-and-path
           #:register-room-id-and-path))
(in-package #:lem-rooms-client/buffer)

(defmethod room-id ((buffer buffer))
  (buffer-value buffer 'room-id))

(defmethod (setf room-id) (room-id buffer)
  (setf (buffer-value buffer 'room-id) room-id))

(defmethod path ((buffer buffer))
  (buffer-value buffer 'path))

(defmethod (setf path) (path (buffer buffer))
  (setf (buffer-value buffer 'path) path))

(defun find-buffer-by-room-and-path (room-id path)
  (dolist (buffer (buffer-list))
    (when (and (equal room-id (room-id buffer))
               (equal path (path buffer)))
      (return buffer))))

(defun register-room-id-and-path (buffer room-id path)
  (setf (room-id buffer) room-id
        (path buffer) path))
