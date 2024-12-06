(uiop:define-package #:lem-rooms-client
  (:use #:cl
        #:alexandria
        #:lem
        #:lem-rooms-client/utils
        #:lem-rooms-client/agent
        #:lem-rooms-client/room
        #:lem-rooms-client/user)
  (:local-nicknames (#:cursor #:lem-rooms-client/cursor)
                    (#:config #:lem-rooms-client/config)
                    (#:rooms-api #:lem-rooms-client/rooms-api)
                    (#:agent-api #:lem-rooms-client/agent-api)
                    (#:sign-in #:lem-rooms-client/sign-in)
                    (#:buffer #:lem-rooms-client/buffer)
                    (#:management-buffer #:lem-rooms-client/management-buffer)
                    (#:client #:lem-rooms-client/client)
                    (#:connected-hook #:lem-rooms-client/connected-hook)))
(in-package #:lem-rooms-client)

(defun create-rooms-pane ()
  (let ((buffer (make-buffer "*Rooms right-side-pane*" :temporary t :enable-undo-p nil)))
    (setf (not-switchable-buffer-p buffer) t)
    (management-buffer:update buffer :status :connecting)
    (make-rightside-window buffer :width 30)
    buffer))

(defun open-room-directory (directory)
  (find-file directory))

(defun start-room (room)
  (management-buffer:update (room-management-buffer room) :status :connected)
  (open-room-directory (room-directory room)))

(defun enter-room (room-id websocket-url &key then)
  (let* ((enter-room-result (agent-api:enter-room :room-id room-id
                                                  :user-name (config:user-name)
                                                  :websocket-url websocket-url))
         (client-id (gethash "clientID" enter-room-result)))
    (connected-hook:add (lambda ()
                          (funcall then client-id)))))

(defun prompt-for-scope (prompt)
  (prompt-for-string prompt
                     :initial-value "public"
                     :completion-function (lambda (s)
                                            (completion s '("public" "private")))
                     :test-function (lambda (s)
                                      (member s '("public" "private") :test #'equal))))

(define-command rooms-create-room () ()
  (client:init)
  (let* ((room-name (prompt-for-string "Room name: "
                                       :test-function (lambda (string)
                                                        (< 0 (length string)))))
         (scope (prompt-for-scope "Room scope: "))
         (directory (prompt-for-directory "Share directory: "
                                          :existing t
                                          :directory (buffer-directory)))
         (room (rooms-api:create-room :name room-name :scope scope :access-token (config:access-token))))
    (let ((room-id (rooms-api:room-id room))
          (management-buffer (create-rooms-pane)))
      (enter-room room-id
                  (rooms-api:room-websocket-url room)
                  :then (lambda (client-id)
                          (agent-api:share-directory :room-id room-id :path directory)
                          (start-room
                           (register-room
                            :room-id room-id
                            :client-id client-id
                            :directory directory
                            :management-buffer management-buffer
                            :owner-p t)))))))

(defun join-room (room-json)
  (let* ((room-id (rooms-api:room-id room-json))
         (room (find-room-by-id room-id)))
    (if room
        (open-room-directory (room-directory room))
        (let ((management-buffer (create-rooms-pane)))
          (enter-room room-id
                      (rooms-api:room-websocket-url room-json)
                      :then (lambda (client-id)
                              (let ((directory (agent-api:sync-directory :room-id room-id)))
                                (start-room
                                 (register-room
                                  :client-id client-id
                                  :room-id room-id
                                  :directory (namestring
                                              (uiop:ensure-directory-pathname directory))
                                  :management-buffer management-buffer
                                  :owner-p nil)))))))))

(define-command rooms-list () ()
  (client:init)
  (lem/multi-column-list:display
   (make-instance 'lem/multi-column-list:multi-column-list
                  :columns '("Room" "Scope" "Owner" "Users")
                  :column-function (lambda (component room)
                                     (declare (ignore component))
                                     (list (format nil "~A  " (rooms-api:room-name room))
                                           (format nil "~A  " (rooms-api:room-scope room))
                                           (format nil
                                                   "~A  "
                                                   (rooms-api:user-github-login
                                                    (rooms-api:room-owner room)))
                                           (format nil
                                                   "~{~A ~}"
                                                   (mapcar #'rooms-api:user-github-login
                                                           (rooms-api:room-users room)))))
                  :items (rooms-api:get-rooms :access-token (config:access-token))
                  :select-callback (lambda (component room)
                                     (start-timer (make-idle-timer (lambda ()
                                                                     (join-room room)))
                                                  0)
                                     (lem/multi-column-list:quit component)))))

(defparameter *recreation-invitation-code-message*
  (format nil "An invitation code has already been issued.~@
               Do you want to invalidate the old invitation code and create a new one?"))

(define-command rooms-publish-invitation () ()
  (let ((room (find-room-by-file (buffer-directory (current-buffer)))))
    (unless (and room (room-owner-p room))
      (editor-error "Only the room owner can issue invitations"))
    (let* ((invitation (if (or (null (room-invitation room))
                               (prompt-for-y-or-n-p *recreation-invitation-code-message*))
                           (rooms-api:create-invitation (room-id room) :access-token (config:access-token))
                           (room-invitation room)))
           (code (gethash "code" invitation)))
      (show-message (format nil " Invitation code: ~A ~2% copied to clipboard" code)
                    :style '(:gravity :center)
                    :timeout nil)
      (copy-to-clipboard code)
      (setf (room-invitation room) invitation))))

(define-command rooms-join-by-invitation-code (invitation-code) ((:string "Invitation code: "))
  (client:init)
  (let ((room-json (rooms-api:get-room-by-invitation invitation-code :access-token (config:access-token))))
    (join-room room-json)))
