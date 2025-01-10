(uiop:define-package #:rooms-client/agent-api
  (:use #:cl)
  (:local-nicknames (#:agent #:rooms-client/agent))
  (:shadow #:room)
  (:export #:user
           #:user-id
           #:user-github-login
           #:user-avatar-url
           #:room
           #:room-id
           #:room-name
           #:room-owner
           #:room-users
           #:room-scope
           #:room-websocket-url
           #:user-state
           #:user-state-client-id
           #:user-state-name
           #:user-state-color
           #:user-state-room-id
           #:user-state-path
           #:user-state-position
           #:user-state-active
           #:user-state-myself
           #:authenticated
           #:authenticated-access-token
           #:invitation
           #:invitation-owner
           #:invitation-code
           #:entered-room
           #:entered-room-client-id
           #:commented-user
           #:commented-user-name
           #:commented-user-color
           #:comment
           #:comment-user
           #:comment-text
           #:comment-date
           #:commented-event
           #:commented-event-added
           #:commented-event-room-id
           #:convert-to-user
           #:convert-to-room
           #:convert-to-user-state
           #:convert-to-authenticated
           #:convert-to-invitation
           #:convert-to-entered-room
           #:convert-to-commented-user
           #:convert-to-comment
           #:convert-to-commented-event
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

(defgeneric convert (name value))

(defmacro define-json-structure ((name &key converter) &body slots)
  (alexandria:with-unique-names (structure-name value)
    `(progn
       (defstruct ,name
         ,@(loop :for (slot) :in slots
                 :collect slot))
       (defmethod convert ((,structure-name (eql ',name)) ,value)
         (,(alexandria:symbolicate 'make- name)
          ,@(loop :for (slot-name field-name &key slot-converter) :in slots
                  :append (list (alexandria:make-keyword slot-name)
                                (if slot-converter
                                    `(,slot-converter (gethash ,field-name ,value))
                                    `(gethash ,field-name ,value))))))
       (defun ,converter (,value)
         (convert ',name ,value)))))

(define-json-structure (user :converter convert-to-user)
  (id "id")
  (github-login "name")
  (avatar-url "avatar_url"))

(define-json-structure (room :converter convert-to-room)
  (id "id")
  (name "name")
  (owner "owner" :converter convert-to-user)
  (users "users" :converter (lambda (value)
                              (map 'list #'convert-to-user value)))
  (scope "scope")
  (websocket-url "websocket_url"))

(define-json-structure (user-state :converter convert-to-user-state)
  (client-id "clientId")
  (name "name")
  (color "color")
  (room-id "roomId")
  (path "path")
  (position "position")
  (active "active")
  (myself "myself"))

(define-json-structure (authenticated :converter convert-to-authenticated)
  (access-token "access_token"))

(define-json-structure (invitation :converter convert-to-invitation)
  (owner "owner" :converter convert-to-user)
  (code "code"))

(define-json-structure (entered-room :converter convert-to-entered-room)
  (client-id "clientID"))

(define-json-structure (commented-user :converter convert-to-commented-user)
  (name "name")
  (color "color"))

(define-json-structure (comment :converter convert-to-comment)
  (user "user" :converter convert-to-commented-user)
  (text "text")
  (date "date"))

(define-json-structure (commented-event :converter convert-to-commented-event)
  (added "added" :converter (lambda (value) (map 'list #'convert-to-comment value)))
  (room-id "roomId"))

(defun sign-in (agent &key name)
  (convert-to-authenticated (agent:call agent "rooms/sign-in" (hash :name name))))

(defun get-github-authorize-url (agent)
  (let ((response (agent:call agent "rooms/github-authorize-url" (hash))))
    (gethash "url" response)))

(defun authenticate (agent code)
  (let ((response (agent:call agent "rooms/github-authenticate" (hash :code code))))
    (convert-to-authenticated response)))

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
  (convert-to-invitation
   (agent:call agent
               "rooms/create-invitation"
               (hash :room-id room-id
                     :access-token access-token))))

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
                      :position position))
  (values))

(defun edit (agent &key room-id path ops)
  (agent:call agent
              "edit"
              (hash :room-id room-id
                    :path path
                    :ops ops))
  (values))

(defun enter-room (agent &key room-id user-id user-name websocket-url access-token)
  (convert-to-entered-room
   (agent:call agent
               "enter-room"
               (hash :room-id room-id
                     :user-id user-id
                     :user-name user-name
                     :websocket-url websocket-url
                     :access-token access-token))))

(defun share-directory (agent &key room-id path)
  (agent:notify agent
                "share-directory"
                (hash :room-id room-id
                      :path path))
  (values))

(defun open-file (agent &key room-id path text)
  (let ((text (agent:call agent
                          "open-file"
                          (hash :room-id room-id
                                :path path
                                :text text))))
    text))

(defun sync-directory (agent &key room-id)
  (let ((directory-name (agent:call agent
                                    "sync-directory"
                                    (hash :room-id room-id))))
    directory-name))

(defun comment (agent &key room-id text)
  (agent:notify agent
                "comment"
                (hash :room-id room-id
                      :text text))
  (values))

(defun get-comments (agent &key room-id)
  (map 'list
       #'convert-to-commented-user
       (agent:call agent
                   "get-comments"
                   (hash :room-id room-id))))

(defun get-users (agent &key room-id)
  (map 'list
       #'convert-to-user-state
       (agent:call agent "get-users" (hash :room-id room-id))))

(defun get-text (agent &key room-id path)
  (let ((text (agent:call agent
                          "testing/get-text"
                          (hash :room-id room-id
                                :path path))))
    text))

;;; utils
(defun hash (&rest plist)
  (let ((hash (make-hash-table :test 'equal)))
    (loop :for (key value) :on plist :by #'cddr
          :do (setf (gethash (change-case:camel-case (string key))
                             hash)
                    value))
    hash))
