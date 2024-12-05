(uiop:define-package #:lem-rooms-client/room
  (:use #:cl
        #:alexandria)
  (:shadow #:room)
  (:export #:room-management-buffer
           #:room-client-id
           #:room-id
           #:room-directory
           #:room-owner-p
           #:register-room
           #:find-room-by-id
           #:find-room-by-file))
(in-package #:lem-rooms-client/room)

(defvar *rooms* '())

(defstruct room
  id
  client-id
  directory
  management-buffer
  owner-p)

(defun register-room (&key room-id client-id directory management-buffer owner-p)
  (let ((room (make-room :id room-id
                         :client-id client-id
                         :directory directory
                         :management-buffer management-buffer
                         :owner-p owner-p)))
    (push room *rooms*)
    room))

(defun find-room-by-id (room-id)
  (find room-id *rooms* :key #'room-id :test #'equal))

(defun find-room-by-file (file)
  (dolist (room *rooms*)
    (let ((root-path (namestring (room-directory room)))
          (sub-path (namestring file)))
      (when (starts-with-subseq root-path sub-path)
        (return room)))))
