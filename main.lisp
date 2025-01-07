(uiop:define-package #:lem-rooms-client
  (:use #:cl
        #:alexandria
        #:lem
        #:lem-rooms-client/utils
        #:lem-rooms-client/editor
        #:lem-rooms-client/agent
        #:lem-rooms-client/room
        #:lem-rooms-client/user
        #:lem-rooms-client/defcommand)
  (:local-nicknames (#:cursor #:lem-rooms-client/cursor)
                    (#:agent #:lem-rooms-client/agent)
                    (#:agent-api #:lem-rooms-client/agent-api)
                    (#:buffer #:lem-rooms-client/buffer)
                    (#:management-pane #:lem-rooms-client/management-pane)
                    (#:api-client #:lem-rooms-client/api-client)
                    (#:connected-hook #:lem-rooms-client/connected-hook)))
(in-package #:lem-rooms-client)

(define-minor-mode rooms-mode
    (:name "Rooms"
     :keymap *rooms-mode-keymap*
     :global t))

(define-key *rooms-mode-keymap* "M-P" 'rooms-command-palette)
(define-key management-pane:*rooms-pane-mode-keymap* "c" 'rooms-comment)

(defun rooms-before-init ()
  (rooms-mode t))

(add-hook *after-init-hook* 'rooms-before-init)

(defun init ()
  (rooms-mode t)
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

(defvar *edit-queue* (sb-concurrency:make-queue :name "lem-rooms-client/edit-queue"))

(defun push-edit (function)
  (sb-concurrency:enqueue function *edit-queue*))

(defun consume-edit-queue ()
  (loop :for function := (sb-concurrency:dequeue *edit-queue*)
        :while function
        :do (funcall function)))

(defvar *inhibit-change-notification* nil)

(defun on-before-change (point arg)
  (unless *inhibit-change-notification*
    (consume-edit-queue)
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
  (push-edit
   (lambda ()
     (let ((*inhibit-change-notification* t)
           (edited nil))
       (with-inhibit-undo ()
         (let ((room-id (gethash "roomId" params))
               (path (gethash "path" params))
               (delta (gethash "delta" params)))
           (when-let (buffer (buffer:find-buffer-by-room-and-path room-id path))
             (with-point ((point (buffer-point buffer)))
               (let ((index 0))
                 (do-sequence (op delta)
                   (cond ((gethash "retain" op)
                          (incf index (gethash "retain" op)))
                         ((gethash "insert" op)
                          (move-to-position* point index)
                          (insert-string point (gethash "insert" op))
                          (incf index (length (gethash "insert" op)))
                          (setf edited t))
                         ((gethash "delete" op)
                          (with-point ((start point)
                                       (end point))
                            (move-to-position* start index)
                            (move-to-position* end (+ index (gethash "delete" op)))
                            (delete-between-points start end))
                          (setf edited t))
                         (t
                          (log:error "unexpected delta: ~A" (pretty-json delta))))))))))
       (when (and edited (zerop (event-queue-length)))
         (notify-focus (current-point))
         (redraw-display)))))
  (send-event (lambda () (consume-edit-queue))))

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
                                            :users (map 'list #'agent-api::convert-to-user-state users)
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
    (declare (agent-api::user-state user))
    (when (and (not (agent-api:user-state-myself user))
               (agent-api:user-state-active user))
      (when-let (buffer (buffer:find-buffer-by-room-and-path (agent-api:user-state-room-id user)
                                                             (agent-api:user-state-path user)))
        (when (eq buffer (window-buffer (current-window)))
          (cursor:set-cursor buffer
                             (agent-api:user-state-id user)
                             (agent-api:user-state-name user)
                             (agent-api:user-state-color user)
                             (lsp-to-lem-position (agent-api:user-state-position user))))))))

(defun on-users (params)
  (send-event
   (lambda ()
     (when-let* ((users (map 'list
                             #'agent-api::convert-to-user-state
                             (remove-if #'null (gethash "users" params))))
                 (room-id (block found
                            (map ()
                                 (lambda (user)
                                   (return-from found
                                     (agent-api:user-state-room-id user)))
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

(defun dump-points (buffer)
  (loop :for point :in (lem/buffer/internal::buffer-points buffer)
        :unless (member point
                        (list (buffer-start-point buffer)
                              (buffer-end-point buffer)))
        :collect (list point (line-number-at-point point) (point-charpos point))))

(defun replace-buffer-text (buffer text)
  (let ((points-and-positions (dump-points buffer)))
    (log:debug points-and-positions)
    (delete-between-points (buffer-start-point buffer)
                           (buffer-end-point buffer))
    (insert-string (buffer-point buffer) text)
    (loop :for (point line charpos) :in points-and-positions
          :do (unless (move-to-line point line)
                (buffer-end point))
              (if (<= charpos (length (line-string point)))
                  (line-offset point 0 charpos)
                  (line-end point)))
    (log:debug (dump-points buffer))))

(defun check-buffer-sync ()
  (let ((buffer (current-buffer)))
    (when-let* ((file (buffer-filename buffer))
                (room (find-room-by-file file))
                (text (agent-api:get-text :room-id (room-id room)
                                          :path (buffer:path buffer))))
      (unless (equal text (buffer-text buffer))
        (replace-buffer-text buffer text)
        (message "Rooms BUG/FIX: The text synchronization had failed, so it was resynchronized")))))

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
      (buffer:register-room-id-and-path buffer room-id path)
      (start-timer (make-idle-timer 'check-buffer-sync) 2000 :repeat t))))

(defun start-room (room)
  (management-pane:connected (room-management-pane room))
  (management-pane:redraw (room-management-pane room))
  (management-pane:open-management-pane room)
  (find-file (room-directory room)))

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

(define-rooms-command rooms-create-room () ()
  "Create a new room"
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
    (cond (room
           (management-pane:open-management-pane room)
           (find-file (room-directory room)))
          (t
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
                        (start-room room)))))))))

(define-rooms-command rooms-list () ()
  "List of rooms"
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

(define-rooms-command rooms-publish-invitation () ()
  "Create an invitation to your room"
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

(define-rooms-command rooms-join-by-invitation-code (invitation-code) ((:string "Invitation code: "))
  "Enter the room where you received the invitation"
  (init)
  (let ((room-json (api-client:join-by-invitation-code (api-client:client) invitation-code)))
    (join-room room-json)))

(define-rooms-command rooms-toggle-pane () ()
  "Show or hide the pane on the right"
  (cond ((management-pane:current-management-pane)
         (close-rightside-window))
        (t
         ;; TODO: 複数のroomを開いている場合にどうするか
         (management-pane:open-management-pane (default-room)))))

(define-rooms-command rooms-sign-in () ()
  "Sign in to Rooms"
  (setf (api-client:client-access-token (api-client:client)) nil) ;TODO: encapsulation
  (init)
  (message "Sign-in Successful"))

(define-command rooms-backdoor (name) ((:string "Name: "))
  (run-agent-if-not-alive)
  (init-editor-hooks)
  (api-client:sign-in-backdoor (api-client:client) name)
  (message "Sign-in Successful"))

(defun get-current-room ()
  (if-let (pane (management-pane:current-management-pane))
    (find-room-by-id (management-pane::management-pane-room-id pane))
    ;; TODO: 複数のroomを開いている場合にどうするか
    (default-room)))

(define-rooms-command rooms-comment () ()
  "Comment in this room"
  (flet ((comment (room)
           (let ((text (prompt-for-string "Comment: "
                                          :test-function (lambda (s) (plusp (length s)))
                                          :gravity :center
                                          :use-border t)))
             (agent-api:comment :room-id (room-id room)
                                :text text))))
    (with-save-cursor (current-buffer)
      (when-let (room (get-current-room))
        (with-current-buffer (management-pane::management-pane-buffer (room-management-pane room))
          (if (frame-rightside-window (current-frame))
              (with-current-window (frame-rightside-window (current-frame))
                (comment room))
              (comment room)))))))

(defun choose-user (user-states)
  (if (length= 1 user-states)
      (first user-states)
      (prompt-for-string
       "To which user?: "
       :completion-function (lambda (s)
                              (let ((result
                                      (completion-strings s
                                                          user-states
                                                          :key #'agent-api:user-state-name)))
                                (mapcar #'agent-api:user-state-name result)))
       :test-function (lambda (s)
                        (member s
                                user-states
                                :key #'agent-api:user-state-name
                                :test #'equal)))))

(defun jump-to (user-state room)
  (let ((path (agent-api:user-state-path user-state))
        (position (agent-api:user-state-position user-state)))
    (find-file (merge-pathnames path (room-directory room)))
    (move-to-position* (current-point) position)))

(define-rooms-command rooms-jump-to-other-user-cursor () ()
  "Jump to the cursor position of other user"
  (when-let (room (get-current-room))
    (let ((user-state
            (choose-user (remove-if #'agent-api:user-state-myself
                                    (agent-api:get-users :room-id (room-id room))))))
      (jump-to user-state room))))

(define-command rooms-command-palette (arg) (:universal-nil)
  (let* ((commands (list-rooms-commands))
         (completions (loop :for command :in commands
                            :collect (lem/completion-mode:make-completion-item
                                      :label (rooms-command-name command)
                                      :detail (rooms-command-description command)))))
    (let ((command
            (prompt-for-string
             "Rooms Command: "
             :completion-function (lambda (s)
                                    (completion-strings
                                     s
                                     completions
                                     :key #'lem/completion-mode:completion-item-label))
             :test-function (lambda (s)
                              (member s
                                      completions
                                      :test #'equal
                                      :key #'lem/completion-mode:completion-item-label))
             :history-symbol 'rooms-command-palette
             :syntax-table lem-lisp-syntax:*syntax-table*)))
      (call-command (find-command command) arg))))
