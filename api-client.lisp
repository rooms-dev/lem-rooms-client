(uiop:define-package #:lem-rooms-client/api-client
  (:use #:cl)
  (:local-nicknames (#:config #:lem-rooms-client/config)
                    (#:rooms-api #:lem-rooms-client/rooms-api)
                    (#:sign-in #:lem-rooms-client/sign-in)
                    (#:rooms-api #:lem-rooms-client/rooms-api))
  (:export #:client
           #:client-access-token
           #:user-name
           #:init
           #:sign-in
           #:sign-in-backdoor
           #:create-room
           #:get-rooms
           #:create-invitation
           #:join-by-invitation-code))
(in-package #:lem-rooms-client/api-client)

(defvar *client* nil)

(defun client ()
  (or *client*
      (setf *client* (make-instance 'client))))

(defclass client ()
  ((access-token :initform (lem:config :rooms.access-token)
                 :accessor client-access-token)
   (user :initform (lem:config :room.user)
         :accessor client-user)))

(defmethod (setf client-access-token) :before (token (client client))
  (setf (lem:config :rooms.access-token) token))

(defmethod (setf client-user) :before (user (client client))
  (assert (getf user :id))
  (assert (getf user :github-login))
  (assert (getf user :avatar-url))
  (setf (lem:config :room.user) user))

(defmethod user-name ((client client))
  (getf (client-user client) :github-login))

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

(defmethod create-room ((client client) &key name scope)
  (rooms-api:create-room :name name
                         :scope scope
                         :access-token (client-access-token client)))

(defmethod get-rooms ((client client))
  (rooms-api:get-rooms :access-token (client-access-token client)))

(defmethod create-invitation ((client client) room-id)
  (rooms-api:create-invitation room-id
                               :access-token (client-access-token client)))

(defmethod join-by-invitation-code ((client client) invitation-code)
  (rooms-api:get-room-by-invitation invitation-code
                                    :access-token (client-access-token client)))

