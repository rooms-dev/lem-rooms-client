(uiop:define-package #:rooms-client/client
  (:use #:cl)
  (:local-nicknames (#:agent #:rooms-client/agent)
                    (#:agent-api #:rooms-client/agent-api))
  (:export #:client
           #:client-access-token
           #:client-agent
           #:client-user
           #:launch
           #:user-name
           #:sign-in-if-required
           #:sign-in
           #:sign-in-backdoor
           #:sign-out
           #:create-room
           #:get-rooms
           #:create-invitation
           #:join-by-invitation-code
           #:enter-room
           #:sync-directory
           #:get-comments
           #:comment))
(in-package #:rooms-client/client)

(defgeneric sign-in (client))

(defclass client ()
  ((access-token :initarg :access-token
                 :initform nil
                 :accessor client-access-token)
   (user :initarg :user
         :initform nil
         :accessor client-user)
   (agent :initarg :agent
          :reader client-agent)))

(defun launch (&key on-message
                    on-connected
                    on-disconnected
                    on-edit
                    on-users
                    on-comments
                    on-file-changed
                    access-token
                    user
                    (class-name 'client))
  (let* ((agent (agent:run-agent :on-message on-message
                                 :on-connected on-connected
                                 :on-disconnected on-disconnected
                                 :on-edit on-edit
                                 :on-users on-users
                                 :on-comments on-comments
                                 :on-file-changed on-file-changed)))
    (make-instance class-name
                   :agent agent
                   :access-token access-token
                   :user user)))

(defmethod user-name ((client client))
  (getf (client-user client) :github-login))

(defmethod sign-in-if-required ((client client))
  (unless (client-access-token client)
    (sign-in client)))

(defmethod sign-in-backdoor ((client client) name)
  (let ((response (agent-api:sign-in (client-agent client) :name name)))
    (setf (client-access-token client)
          (gethash "access_token" response))))

(defmethod sign-in :after ((client client))
  (set-user-if-not-set client))

(defmethod sign-in-backdoor :after ((client client) name)
  (set-user-if-not-set client))

(defmethod set-user-if-not-set ((client client))
  (unless (client-user client)
    (let ((user (agent-api:get-user (client-agent client)
                                    :access-token (client-access-token client))))
      (setf (client-user client)
            (list :id (agent-api:user-id user)
                  :github-login (agent-api:user-github-login user)
                  :avatar-url (agent-api:user-avatar-url user))))))

(defmethod sign-out ((client client))
  (setf (client-user client) nil
        (client-access-token client) nil))

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

(defmethod enter-room ((client client) room)
  (check-type room agent-api:room)
  (agent-api:enter-room (client-agent client)
                        :room-id (agent-api:room-id room)
                        :user-name (user-name client)
                        :websocket-url (agent-api:room-websocket-url room)
                        :access-token (client-access-token client)))

(defmethod sync-directory ((client client) room)
  (agent-api:sync-directory (client-agent client)
                            :room-id (agent-api:room-id room)))

(defmethod get-comments ((client client) room)
  (agent-api:get-comments (client-agent client) :room-id (agent-api:room-id room)))

(defmethod comment ((client client) room text)
  (agent-api:comment (client-agent client) :room-id (agent-api:room-id room) :text text))
