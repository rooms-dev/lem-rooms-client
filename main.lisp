(uiop:define-package #:lem-rooms-client
  (:use #:cl
        #:alexandria
        #:lem
        #:lem-rooms-client/utils
        #:lem-rooms-client/editor
        #:lem-rooms-client/agent
        #:lem-rooms-client/room
        #:lem-rooms-client/user)
  (:local-nicknames (#:cursor #:lem-rooms-client/cursor)
                    (#:agent #:lem-rooms-client/agent)
                    (#:agent-api #:lem-rooms-client/agent-api)
                    (#:buffer #:lem-rooms-client/buffer)
                    (#:management-pane #:lem-rooms-client/management-pane)
                    (#:api-client #:lem-rooms-client/api-client)
                    (#:connected-hook #:lem-rooms-client/connected-hook)))
(in-package #:lem-rooms-client)

(defvar *inhibit-change-notification* nil)

(defun init ()
  (run-agent-if-not-alive)
  (api-client:init (api-client:client))
  (init-editor-hooks))

(defun run-agent-if-not-alive ()
  (unless (agent:agent-alive-p)
    (agent:run-agent :on-message 'on-message
                     :on-connected 'on-connected
                     :on-disconnected 'on-disconnected
                     :on-edit 'on-edit
                     :on-users 'on-users
                     :on-comments 'on-comments
                     :on-file-changed 'on-file-changed)))

(defun init-editor-hooks ()
  (add-hook *post-command-hook* 'on-post-command)
  (add-hook *find-file-hook* 'on-find-file)
  (add-hook *exit-editor-hook* (lambda () (agent:destroy-agent-if-alive)))
  (add-hook (variable-value 'before-change-functions :global t) 'on-before-change))

(defun notify-focus (point)
  (let ((buffer (point-buffer point)))
    (when (buffer:room-id buffer)
      (agent-api:focus :name (api-client:user-name (api-client:client))
                       :room-id (buffer:room-id buffer)
                       :path (buffer:path buffer)
                       :position (position-of point)))))

(defun on-before-change (point arg)
  (unless *inhibit-change-notification*
    (when-let* ((buffer (point-buffer point))
                (room-id (buffer:room-id buffer))
                (position (position-of point)))
      (etypecase arg
        (string
         (agent-api:edit :room-id room-id
                         :path (buffer:path buffer)
                         :ops (vector (hash :range (hash :start position
                                                         :end position)
                                            :text arg))))
        (integer
         (with-point ((end point))
           (unless (character-offset end arg)
             (buffer-end end))
           (agent-api:edit :room-id room-id
                           :path (buffer:path buffer)
                           :ops (vector (hash :range (hash :start position
                                                           :end (position-of end))
                                              :text "")))))))))

(defun on-edit (params)
  (send-event
   (lambda ()
     (let ((*inhibit-change-notification* t)
           (edited nil))
       (with-inhibit-undo ()
         (let ((room-id (gethash "roomId" params))
               (path (gethash "path" params))
               (ops (gethash "ops" params)))
           (when-let (buffer (buffer:find-buffer-by-room-and-path room-id path))
             (with-point ((point (buffer-point buffer)))
               (do-sequence (op ops)
                 (let* ((range (gethash "range" op))
                        (start (gethash "start" range))
                        (end (gethash "end" range))
                        (text (gethash "text" op)))
                   (unless (= start end)
                     (with-point ((start-point point)
                                  (end-point point))
                       (move-to-position* start-point start)
                       (move-to-position* end-point end)
                       (delete-between-points start-point end-point)
                       (setf edited t)))
                   (unless (emptyp text)
                     (move-to-position* point start)
                     (insert-string point text)
                     (setf edited t))))))))
       (when edited
         (when (zerop (lem::event-queue-length))
           (notify-focus (current-point))
           (redraw-display)))))))

(defun on-message (params)
  (send-event (lambda ()
                (message "agent: ~A" (gethash "message" params)))))

(defun on-connected (params)
  (let ((room-id (gethash "roomId" params))
        (users (gethash "users" params))
        (comments (gethash "comments" params)))
    (send-event (lambda ()
                  (connected-hook:on-connect)
                  (let ((pane (room-management-pane (find-room-by-id room-id))))
                    (management-pane:connected pane)
                    (management-pane:redraw pane
                                            :users users
                                            :adding-comments (management-pane:convert-comments
                                                              comments))
                    (redraw-display))))))

(defun on-disconnected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (let ((pane (room-management-pane (find-room-by-id room-id))))
                    (management-pane:disconnected pane)
                    (connected-hook:disconnect)
                    (management-pane:redraw pane :users '())
                    (redraw-display))))))

(defun update-cursors (users)
  (do-sequence (user users)
    (let ((id (gethash "id" user))
          (name (gethash "name" user))
          (color (gethash "color" user))
          (room-id (gethash "roomId" user))
          (path (gethash "path" user))
          (position (gethash "position" user))
          (active-p (gethash "active" user))
          (myself (gethash "myself" user)))
      (when (and (not myself) active-p)
        (when-let (buffer (buffer:find-buffer-by-room-and-path room-id path))
          (when (eq buffer (window-buffer (current-window)))
            (cursor:set-cursor buffer id name color (lsp-to-lem-position position))))))))

(defun on-users (params)
  (send-event
   (lambda ()
     (when-let* ((users (remove-if #'null (gethash "users" params)))
                 (room-id (block found
                            (map ()
                                 (lambda (user)
                                   (return-from found (gethash "roomId" user)))
                                 users)))
                 (room (find-room-by-id room-id)))
       (update-cursors users)
       (management-pane:redraw (room-management-pane room) :users users)
       (redraw-display)))))

(defun on-comments (params)
  (send-event
   (lambda ()
     (when-let ((room (find-room-by-id (gethash "roomId" params))))
       (management-pane:redraw
        (room-management-pane room)
        :adding-comments (management-pane:convert-comments
                          (gethash "added" params)))))))

(defun on-file-changed (params)
  (send-event
   (lambda ()
     (when-let ((room (find-room-by-id (gethash "roomId" params))))
       ;; BUG:
       ;; 後から入室した時、enter-roomを呼び出す前にfileChangedイベントが飛んでくるが、
       ;; その時点ではまだset-room-directoryが呼ばれてないのでroom-directoryがnilを返しエラーになる
       (when (room-directory room)
         (dolist (file (gethash "added" params))
           (let ((file (merge-pathnames file (room-directory room))))
             (unless (uiop:file-exists-p file)
               (alexandria:write-string-into-file ""
                                                  (ensure-directories-exist file)
                                                  :if-does-not-exist :create)))))))))

(defun on-post-command ()
  (notify-focus (current-point)))

(defun on-find-file (buffer)
  (when-let (room (find-room-by-file (buffer-filename buffer)))
    (let* ((room-id (room-id room))
           (path (namestring
                  (enough-namestring (buffer-filename buffer)
                                     (room-directory room))))
           (text (agent-api:open-file :room-id room-id
                                      :path path
                                      :text (buffer-text buffer))))
      (when (and text (string/= text (buffer-text buffer)))
        (erase-buffer buffer)
        (insert-string (buffer-point buffer) text)
        (buffer-start (buffer-point buffer))
        (when (buffer-enable-undo-p buffer)
          (buffer-disable-undo buffer)
          (buffer-enable-undo buffer)))
      (buffer:register-room-id-and-path buffer room-id path))))

(defun start-room (room)
  (management-pane:connected (room-management-pane room))
  (management-pane:redraw (room-management-pane room))
  (management-pane:open-management-pane room))

(defun enter-room (&key room-id websocket-url then)
  (agent-api:enter-room
   :room-id room-id
   :user-name (api-client:user-name (api-client:client))
   :websocket-url websocket-url
   :access-token (api-client:client-access-token (api-client:client)))
  (connected-hook:add (lambda ()
                        (funcall then))))

(defun prompt-for-scope (prompt)
  (prompt-for-string prompt
                     :initial-value "public"
                     :completion-function (lambda (s)
                                            (completion s '("public" "private")))
                     :test-function (lambda (s)
                                      (member s '("public" "private") :test #'equal))))

(define-command rooms-create-room () ()
  (init)
  (let* ((room-name (prompt-for-string "Room name: "
                                       :test-function (lambda (string)
                                                        (< 0 (length string)))))
         (scope (prompt-for-scope "Room scope: "))
         (directory (prompt-for-directory "Share directory: "
                                          :existing t
                                          :directory (buffer-directory)))
         (room-json (api-client:create-room (api-client:client) :scope scope :name room-name))
         (room-id (agent-api:room-id room-json))
         (management-pane (management-pane:create-pane room-id))
         (room (register-room
                :room-id room-id
                :room-name (agent-api:room-name room-json)
                :directory directory
                :management-pane management-pane
                :owner-p t)))
    (management-pane:connecting management-pane)
    (management-pane:redraw management-pane)
    (enter-room
     :room-id room-id
     :websocket-url (agent-api:room-websocket-url room-json)
     :then (lambda ()
             (agent-api:share-directory :room-id room-id :path directory)
             (start-room room)))))

(defun join-room (room-json)
  (let* ((room-id (agent-api:room-id room-json))
         (room (find-room-by-id room-id)))
    (if room
        (management-pane:open-management-pane room)
        (let* ((management-pane (management-pane:create-pane room-id))
               (room (register-room
                      :room-id room-id
                      :room-name (agent-api:room-name room-json)
                      :management-pane management-pane
                      :owner-p nil)))
          (management-pane:connecting management-pane)
          (management-pane:redraw management-pane)
          (enter-room
           :room-id room-id
           :websocket-url (agent-api:room-websocket-url room-json)
           :then (lambda ()
                   (let ((directory (agent-api:sync-directory :room-id room-id)))
                     (assert directory)
                     (set-room-directory room directory)
                     (start-room room))))))))

(define-command rooms-list () ()
  (init)
  (lem/multi-column-list:display
   (make-instance 'lem/multi-column-list:multi-column-list
                  :columns '("Room" "Scope" "Owner" "Users")
                  :column-function (lambda (component room)
                                     (declare (ignore component))
                                     (list (format nil "~A  " (agent-api:room-name room))
                                           (format nil "~A  " (agent-api:room-scope room))
                                           (format nil
                                                   "~A  "
                                                   (agent-api:user-github-login
                                                    (agent-api:room-owner room)))
                                           (format nil
                                                   "~{~A ~}"
                                                   (mapcar #'agent-api:user-github-login
                                                           (agent-api:room-users room)))))
                  :items (api-client:get-rooms (api-client:client))
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
                           (api-client:create-invitation (api-client:client)
                                                         (room-id room))
                           (room-invitation room)))
           (code (gethash "code" invitation)))
      (show-message (format nil " Invitation code: ~A ~2% copied to clipboard" code)
                    :style '(:gravity :center)
                    :timeout nil)
      (copy-to-clipboard code)
      (setf (room-invitation room) invitation))))

(define-command rooms-join-by-invitation-code (invitation-code) ((:string "Invitation code: "))
  (init)
  (let ((room-json (api-client:join-by-invitation-code (api-client:client) invitation-code)))
    (join-room room-json)))

(define-command rooms-toggle-pane () ()
  (cond ((management-pane:current-management-pane)
         (close-rightside-window))
        (t
         ;; TODO: 複数のroomを開いている場合にどうするか
         (management-pane:open-management-pane (first lem-rooms-client/room::*rooms*)))))

(define-command rooms-sign-in () ()
  (setf (api-client:client-access-token (api-client:client)) nil) ;TODO: encapsulation
  (init)
  (message "Sign-in Successful"))

(define-command rooms-backdoor (name) ((:string "Name: "))
  (run-agent-if-not-alive)
  (init-editor-hooks)
  (api-client:sign-in-backdoor (api-client:client) name)
  (message "Sign-in Successful"))
