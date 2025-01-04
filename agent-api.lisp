(defpackage #:lem-rooms-client/agent-api
  (:use #:cl
        #:lem-rooms-client/utils)
  (:local-nicknames (#:agent #:lem-rooms-client/agent))
  (:shadow #:room)
  (:export #:user-id
           #:user-github-login
           #:user-avatar-url
           #:room-id
           #:room-name
           #:room-owner
           #:room-users
           #:room-scope
           #:room-websocket-url
           #:sign-in
           #:get-github-authorize-url
           #:authenticate
           #:get-user
           #:get-rooms
           #:create-room
           #:create-invitation
           #:get-room-by-invitation
           #:focus
           #:edit
           #:enter-room
           #:share-directory
           #:open-file
           #:sync-directory
           #:comment
           #:get-comments
           #:get-text))
(in-package #:lem-rooms-client/agent-api)

(defstruct user
  id
  github-login
  avatar-url)

(defun convert-to-user (value)
  (make-user :id (gethash "id" value)
             :github-login (gethash "name" value)
             :avatar-url (gethash "avatar_url" value)))

(defstruct room
  id
  name
  owner
  users
  scope
  websocket-url)

(defun convert-to-room (value)
  (make-room :id (gethash "id" value)
             :name (gethash "name" value)
             :owner (convert-to-user (gethash "owner" value))
             :users (mapcar #'convert-to-user (gethash "users" value))
             :scope (gethash "scope" value)
             :websocket-url (gethash "websocket_url" value)))

(defun sign-in (&key name)
  (agent:call "rooms/sign-in" (hash :name name)))

(defun get-github-authorize-url ()
  (let ((response (agent:call "rooms/github-authorize-url" (hash))))
    (gethash "url" response)))

(defun authenticate (code)
  (let ((response (agent:call "rooms/github-authenticate" (hash :code code))))
    response))

(defun get-user (&key access-token)
  (convert-to-user (agent:call "rooms/get-user" (hash :access-token access-token))))

(defun get-rooms (&key access-token)
  (mapcar #'convert-to-room
          (agent:call "rooms/get-rooms"
                      (hash :access-token access-token))))

(defun create-room (&key access-token name scope)
  (convert-to-room
   (agent:call "rooms/create-room"
               (hash :name name
                     :scope scope
                     :access-token access-token))))

(defun create-invitation (&key room-id access-token)
  (agent:call "rooms/create-invitation"
              (hash :room-id room-id
                    :access-token access-token)))

(defun get-room-by-invitation (&key invitation-code access-token)
  (convert-to-room
   (agent:call "rooms/get-room-by-invitation"
               (hash :invitation-code invitation-code
                     :access-token access-token))))

(defun focus (&key name room-id path position)
  (agent:notify "focus"
                (hash :name name
                      :room-id room-id
                      :path path
                      :position position)))

(defun edit (&key room-id path ops)
  (agent:call "edit"
              (hash :room-id room-id
                    :path path
                    :ops ops)))

(defun enter-room (&key room-id user-name websocket-url access-token)
  (agent:call "enter-room"
              (hash :room-id room-id
                    :user-name user-name
                    :websocket-url websocket-url
                    :access-token access-token)))

(defun share-directory (&key room-id path)
  (agent:notify "share-directory"
                (hash :room-id room-id
                      :path path)))

(defun open-file (&key room-id path text)
  (agent:call "open-file"
              (hash :room-id room-id
                    :path path
                    :text text)))

(defun sync-directory (&key room-id)
  (agent:call "sync-directory"
              (hash :room-id room-id)))

(defun comment (&key room-id text)
  (agent:notify "comment"
                (hash :room-id room-id
                      :text text)))

(defun get-comments (&key room-id)
  (agent:call "get-comments"
              (hash :room-id room-id)))

(defun get-text (&key room-id path)
  (agent:call "testing/get-text"
              (hash :room-id room-id
                    :path path)))
