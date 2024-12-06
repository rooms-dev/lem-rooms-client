(defpackage #:lem-rooms-client/agent-api
  (:use #:cl
        #:lem-rooms-client/utils)
  (:local-nicknames (#:agent #:lem-rooms-client/agent))
  (:export #:focus
           #:edit
           #:enter-room
           #:share-directory
           #:open-file
           #:sync-directory
           #:comment
           #:get-comments))
(in-package #:lem-rooms-client/agent-api)

(defun focus (&key name room-id path position)
  (agent:notify "focus"
                (hash :name name
                      :room-id room-id
                      :path path
                      :position position)))

(defun edit (&key room-id path ops)
  (agent:notify "edit"
                (hash :room-id room-id
                      :path path
                      :ops ops)))

(defun enter-room (&key room-id user-name websocket-url)
  (agent:call "enter-room"
              (hash :room-id room-id
                    :user-name user-name
                    :websocket-url websocket-url)))

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
