(uiop:define-package #:lem-rooms-client/rooms-api
  (:use #:cl)
  (:import-from #:lem-rooms-client/utils
                #:hash)
  (:shadow #:get
           #:room)
  (:export #:user-id
           #:user-github-login
           #:user-avatar-url
           #:room-id
           #:room-name
           #:room-owner
           #:room-users
           #:room-scope
           #:room-websocket-url
           #:authenticated-access-token
           #:get
           #:post
           #:authenticate
           #:get-user
           #:get-rooms
           #:create-room
           #:backdoor
           #:create-invitation
           #:get-room-by-invitation))
(in-package #:lem-rooms-client/rooms-api)

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

(defstruct authenticated
  access-token)

(defun convert-to-authenticated (value)
  (make-authenticated :access-token (gethash "access_token" value)))

(defun url (path)
  (quri:make-uri :defaults (lem-rooms-client/config:rooms-url)
                 :path path))

(defun headers (access-token)
  `(("content-type" . "application/json")
    ,@(when access-token
        `(("Authorization" . ,(format nil "Bearer ~A" access-token))))))

(defun content (&rest args)
  (with-output-to-string (out)
    (yason:encode (apply #'hash args) out)))

(defun get (path &key access-token)
  (yason:parse (dex:get (url path)
                        :headers (headers access-token))))

(defun post (path content &key access-token)
  (yason:parse (dex:post (url path)
                         :headers (headers access-token)
                         :content content)))

(defun authenticate (code)
  (let ((response (get (format nil "/github/authenticate?code=~A" code))))
    (convert-to-authenticated response)))

(defun get-user (access-token)
  (convert-to-user (get "/user" :access-token access-token)))

(defun get-rooms (&key access-token)
  (mapcar #'convert-to-room (get "/rooms" :access-token access-token)))

(defun create-room (&key access-token name scope)
  (convert-to-room
   (post "/rooms"
         (content :name name :scope scope)
         :access-token access-token)))

(defun backdoor (name)
  (post "/backdoor"
        (content :name name)))

(defun create-invitation (room-id &key access-token)
  (post (format nil "/rooms/~A/invitations" room-id)
        (content)
        :access-token access-token))

(defun get-room-by-invitation (invitation-code &key access-token)
  (convert-to-room
   (get (format nil "/invitations/~A/room" invitation-code)
        :access-token access-token)))
