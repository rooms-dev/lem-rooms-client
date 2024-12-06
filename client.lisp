(uiop:define-package #:lem-rooms-client/client
  (:use #:cl
        #:lem)
  (:local-nicknames (#:agent #:lem-rooms-client/agent)
                    (#:cursor #:lem-rooms-client/cursor)
                    (#:config #:lem-rooms-client/config)
                    (#:rooms-api #:lem-rooms-client/rooms-api)
                    (#:agent-api #:lem-rooms-client/agent-api)
                    (#:sign-in #:lem-rooms-client/sign-in)
                    (#:buffer #:lem-rooms-client/buffer)
                    (#:management-buffer #:lem-rooms-client/management-buffer))
  (:export #:init))
(in-package #:lem-rooms-client/client)

(defun ensure-sign-in ()
  (unless (config:access-token)
    (sign-in:rooms-sign-in))
  (config:access-token))

(defun setup-agent (access-token)
  (unless (agent:agent-alive-p)
    (agent:run-agent :access-token access-token
                     :on-message 'on-message
                     :on-connected 'on-connected
                     :on-disconnected 'on-disconnected
                     :on-edit 'on-edit
                     :on-users 'on-users
                     :on-comments 'on-comments)))

(defun set-user-if-not-set (access-token)
  (unless (config:user)
    (let ((user (rooms-api:get-user access-token)))
      (setf (config:user)
            (list :id (rooms-api:user-id user)
                  :github-login (rooms-api:user-github-login user)
                  :avatar-url (rooms-api:user-avatar-url user))))))

(defun init ()
  (let ((access-token (ensure-sign-in)))
    (setup-agent access-token)
    (set-user-if-not-set access-token)))
