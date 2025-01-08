(uiop:define-package #:lem-rooms-client/api-client
  (:use #:cl)
  (:local-nicknames (#:agent-api #:rooms-client/agent-api))
  (:export #:client
           #:client-access-token
           #:client-agent
           #:client-user
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

(defgeneric sign-in (client))

(defclass client ()
  ((access-token :initarg :access-token
                 :accessor client-access-token)
   (user :initarg :user
         :accessor client-user)
   (agent :initarg :agent
          :reader client-agent)))

(defmethod user-name ((client client))
  (getf (client-user client) :github-login))

(defmethod sign-in-if-required ((client client))
  (unless (client-access-token client)
    (sign-in client)))

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
