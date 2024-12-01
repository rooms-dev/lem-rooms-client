(uiop:define-package #:lem-rooms-client/rooms-api
  (:use #:cl
        #:lem-rooms-client/config)
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
           #:get
           #:post
           #:get-user
           #:get-rooms
           #:create-room
           #:backdoor))
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

(defun url (path)
  (quri:make-uri :defaults *rooms-url*
                 :path path))

(defun headers (authorization)
  `(("content-type" . "application/json")
    ,@(when authorization
        `(("Authorization" . ,(format nil "Bearer ~A" (access-token)))))))

(defun content (&rest args)
  (with-output-to-string (out)
    (yason:encode (apply #'hash args) out)))

(defun get (path &key authorization)
  (yason:parse (dex:get (url path)
                        :headers (headers authorization))))

(defun post (path content)
  (yason:parse (dex:post (url path)
                         :headers (headers t)
                         :content content)))

(defun get-user ()
  (convert-to-user (get "/user" :authorization t)))

(defun get-rooms ()
  (mapcar #'convert-to-room (get "/rooms")))

(defun create-room (&key name scope)
  (convert-to-room
   (post "/rooms"
         (content :name name :scope scope))))

(defun backdoor (name)
  (post "/backdoor"
        (content :name name)))
