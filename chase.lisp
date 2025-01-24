(uiop:define-package :lem-rooms-client/chase
  (:use #:cl #:lem)
  (:local-nicknames (#:agent-api #:rooms-client/agent-api))
  (:export #:chase-client-id
           #:show-chase-popup-message
           #:remove-chase-popup-message
           #:chase-user-cursor
           #:chase-off
           #:chase))
(in-package :lem-rooms-client/chase)

(defvar *chase-client-id* nil)
(defvar *chase-popup-message* nil)
(defvar *chase-line-overlay* nil)

(defun chase-client-id ()
  *chase-client-id*)

(defun set-chase-client-id (client-id)
  (setf *chase-client-id* client-id))

(defun show-chase-popup-message ()
  (setf *chase-popup-message*
        (display-popup-message "Press any key to exit chase"
                               :timeout nil
                               :style '(:gravity :topright)
                               :destination-window *chase-popup-message*)))

(defun remove-chase-popup-message ()
  (when *chase-popup-message*
    (delete-popup-message *chase-popup-message*)
    (setf *chase-popup-message* nil)))

(defun chase-user-cursor (user-state room)
  (declare (ignore room))
  (set-chase-client-id (agent-api:user-state-client-id user-state))
  (add-hook *pre-command-hook* 'chase-off)
  (add-hook *post-command-hook* 'on-post-command)
  (hide-cursor (current-window)))

(defun chase-off ()
  (set-chase-client-id nil)
  (remove-hook *pre-command-hook* 'chase-off)
  (remove-hook *post-command-hook* 'on-post-command)
  (show-cursor (current-window))
  (remove-chase-popup-message)
  (clear-chase-overlay))

(defun on-post-command ()
  (when (chase-client-id)
    (show-chase-popup-message)))

(defun clear-chase-overlay ()
  (when *chase-line-overlay*
    (delete-overlay *chase-line-overlay*)
    (setf *chase-line-overlay* nil)))

(defun chase (point user-state)
  (clear-chase-overlay)
  (let ((overlay (make-line-overlay
                  point
                  (make-attribute
                   :background (agent-api:user-state-color user-state)))))
    (setf *chase-line-overlay* overlay)))
