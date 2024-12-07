(uiop:define-package #:lem-rooms-client/api-client
  (:use #:cl)
  (:local-nicknames (#:config #:lem-rooms-client/config)
                    (#:rooms-api #:lem-rooms-client/rooms-api)
                    (#:sign-in #:lem-rooms-client/sign-in))
  (:export #:client
           #:client-access-token
           #:init
           #:sign-in
           #:sign-in-backdoor))
(in-package #:lem-rooms-client/api-client)

(defvar *client* nil)

(defun client ()
  (or *client*
      (setf *client* (make-instance 'client))))

(defclass client ()
  ((access-token :initform (config:access-token)
                 :accessor client-access-token)
   (user :initform (config:user)
         :accessor client-user)))

(defmethod (setf client-access-token) :before (token (client client))
  (setf (config:access-token) token))

(defmethod (setf client-user) :before (user (client client))
  (setf (config:user) user))

(defmethod init ((client client))
  (sign-in client)
  (set-user-if-not-set client))

(defmethod sign-in ((client client))
  (when (client-access-token client)
    (setf (client-access-token client)
          (sign-in:sign-in)))
  (values))

(defmethod sign-in-backdoor ((client client) name)
  (let ((response (rooms-api:backdoor name)))
    (setf (client-access-token client)
          (gethash "access_token" response))))

(defmethod set-user-if-not-set ((client client))
  (unless (client-user client)
    (let ((user (rooms-api:get-user (client-access-token client))))
      (setf (client-user client)
            (list :id (rooms-api:user-id user)
                  :github-login (rooms-api:user-github-login user)
                  :avatar-url (rooms-api:user-avatar-url user))))))
