(uiop:define-package #:lem-rooms-client/api-client
  (:use #:cl)
  (:local-nicknames (#:sign-in #:lem-rooms-client/sign-in)
                    (#:agent-api #:rooms-client/agent-api))
  (:export #:client
           #:new-client
           #:client-access-token
           #:client-connection-status
           #:client-agent
           #:user-name
           #:sign-in-if-required
           #:sign-in
           #:sign-in-backdoor
           #:set-user-if-not-set
           #:create-room
           #:get-rooms
           #:create-invitation
           #:join-by-invitation-code))
(in-package #:lem-rooms-client/api-client)

(defvar *client* nil)

(defun client ()
  *client*)

(defun new-client (agent)
  (assert (null *client*))
  (setf *client* (make-instance 'client :agent agent)))

(defclass client ()
  ((access-token :initform (lem:config :rooms.access-token)
                 :accessor client-access-token)
   (user :initform (lem:config :room.user)
         :accessor client-user)
   (agent :initarg :agent
          :reader client-agent)))

(defmethod (setf client-access-token) :before (token (client client))
  (setf (lem:config :rooms.access-token) token))

(defmethod (setf client-user) :before (user (client client))
  (assert (getf user :id))
  (assert (getf user :github-login))
  (assert (getf user :avatar-url))
  (setf (lem:config :room.user) user))

(defmethod user-name ((client client))
  (getf (client-user client) :github-login))

(defmethod sign-in-if-required ((client client))
  (unless (client-access-token client)
    (sign-in client)))

(defmethod sign-in ((client client))
  (setf (client-access-token client)
        (sign-in:sign-in (client-agent client)))
  (values))

(defmethod sign-in-backdoor ((client client) name)
  (let ((response (agent-api:sign-in (client-agent client) :name name)))
    (setf (client-access-token client)
          (gethash "access_token" response))))

(defmethod set-user-if-not-set ((client client))
  (unless (client-user client)
    (let ((user (agent-api:get-user (client-agent client)
                                    :access-token (client-access-token client))))
      (setf (client-user client)
            (list :id (agent-api:user-id user)
                  :github-login (agent-api:user-github-login user)
                  :avatar-url (agent-api:user-avatar-url user))))))

(defmethod create-room ((client client) &key name scope)
  (agent-api:create-room (client-agent client)
                         :name name
                         :scope scope
                         :access-token (client-access-token client)))

(defmethod get-rooms ((client client))
  (agent-api:get-rooms (client-agent client)
                       :access-token (client-access-token client)))

(defmethod create-invitation ((client client) room-id)
  (agent-api:create-invitation (client-agent client)
                               :room-id room-id
                               :access-token (client-access-token client)))

(defmethod join-by-invitation-code ((client client) invitation-code)
  (agent-api:get-room-by-invitation (client-agent client)
                                    :invitation-code invitation-code
                                    :access-token (client-access-token client)))
