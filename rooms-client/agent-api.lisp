(defpackage #:rooms-client/agent-api
  (:use #:cl)
  (:local-nicknames (#:agent #:rooms-client/agent))
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
           #:user-state-id
           #:user-state-name
           #:user-state-color
           #:user-state-room-id
           #:user-state-path
           #:user-state-position
           #:user-state-active
           #:user-state-myself
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
           #:get-users
           #:get-text))
(in-package #:rooms-client/agent-api)

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

(defstruct user-state
  id
  name
  color
  room-id
  path
  position
  active
  myself)

(defun convert-to-user-state (value)
  (make-user-state :id (gethash "id" value)
                   :name (gethash "name" value)
                   :color (gethash "color" value)
                   :room-id (gethash "roomId" value)
                   :path (gethash "path" value)
                   :position (gethash "position" value)
                   :active (gethash "active" value)
                   :myself (gethash "myself" value)))

(defun sign-in (agent &key name)
  (agent:call agent "rooms/sign-in" (hash :name name)))

(defun get-github-authorize-url (agent)
  (let ((response (agent:call agent "rooms/github-authorize-url" (hash))))
    (gethash "url" response)))

(defun authenticate (agent code)
  (let ((response (agent:call agent "rooms/github-authenticate" (hash :code code))))
    response))

(defun get-user (agent &key access-token)
  (convert-to-user (agent:call agent "rooms/get-user" (hash :access-token access-token))))

(defun get-rooms (agent &key access-token)
  (mapcar #'convert-to-room
          (agent:call agent 
                      "rooms/get-rooms"
                      (hash :access-token access-token))))

(defun create-room (agent &key access-token name scope)
  (convert-to-room
   (agent:call agent
               "rooms/create-room"
               (hash :name name
                     :scope scope
                     :access-token access-token))))

(defun create-invitation (agent &key room-id access-token)
  (agent:call agent
              "rooms/create-invitation"
              (hash :room-id room-id
                    :access-token access-token)))

(defun get-room-by-invitation (agent &key invitation-code access-token)
  (convert-to-room
   (agent:call agent
               "rooms/get-room-by-invitation"
               (hash :invitation-code invitation-code
                     :access-token access-token))))

(defun focus (agent &key name room-id path position)
  (agent:notify agent
                "focus"
                (hash :name name
                      :room-id room-id
                      :path path
                      :position position)))

(defun edit (agent &key room-id path ops)
  (agent:call agent
              "edit"
              (hash :room-id room-id
                    :path path
                    :ops ops)))

(defun enter-room (agent &key room-id user-name websocket-url access-token)
  (agent:call agent
              "enter-room"
              (hash :room-id room-id
                    :user-name user-name
                    :websocket-url websocket-url
                    :access-token access-token)))

(defun share-directory (agent &key room-id path)
  (agent:notify agent
                "share-directory"
                (hash :room-id room-id
                      :path path)))

(defun open-file (agent &key room-id path text)
  (agent:call agent
              "open-file"
              (hash :room-id room-id
                    :path path
                    :text text)))

(defun sync-directory (agent &key room-id)
  (agent:call agent
              "sync-directory"
              (hash :room-id room-id)))

(defun comment (agent &key room-id text)
  (agent:notify agent
                "comment"
                (hash :room-id room-id
                      :text text)))

(defun get-comments (agent &key room-id)
  (agent:call agent
              "get-comments"
              (hash :room-id room-id)))

(defun get-users (agent &key room-id)
  (map 'list #'convert-to-user-state (agent:call agent "get-users" (hash :room-id room-id))))

(defun get-text (agent &key room-id path)
  (agent:call agent
              "testing/get-text"
              (hash :room-id room-id
                    :path path)))

;;; utils
(defun hash (&rest plist)
  (let ((hash (make-hash-table :test 'equal)))
    (loop :for (key value) :on plist :by #'cddr
          :do (setf (gethash (change-case:camel-case (string key))
                             hash)
                    value))
    hash))
