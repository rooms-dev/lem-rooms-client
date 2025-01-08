(uiop:define-package #:lem-rooms-client/room
  (:use #:cl
        #:alexandria)
  (:shadow #:room)
  (:export #:room-management-pane
           #:room-room
           #:room-id
           #:room-name
           #:room-client-id
           #:room-directory
           #:room-invitation
           #:room-owner-p
           #:register-room
           #:set-room-directory
           #:find-room-by-id
           #:find-room-by-file
           #:default-room))
(in-package #:lem-rooms-client/room)

(defvar *rooms* '())

(defstruct room
  id
  name
  directory
  management-pane
  invitation
  owner-p
  (room nil :type rooms-client/agent-api:room))

(defun register-room (&key room room-name directory management-pane owner-p)
  (let ((room (make-room :id (rooms-client/agent-api:room-id room)
                         :room room
                         :name room-name
                         :directory directory
                         :management-pane management-pane
                         :owner-p owner-p)))
    (push room *rooms*)
    room))

(defmethod set-room-directory ((room room) directory)
  (setf (room-directory room)
        (namestring (uiop:ensure-directory-pathname directory))))

(defun find-room-by-id (room-id)
  (find room-id *rooms* :key #'room-id :test #'equal))

(defun find-room-by-file (file)
  (dolist (room *rooms*)
    (let ((root-path (namestring (room-directory room)))
          (sub-path (namestring file)))
      (when (starts-with-subseq root-path sub-path)
        (return room)))))

(defun default-room ()
  (first *rooms*))
