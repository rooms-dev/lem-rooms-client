(uiop:define-package #:lem-rooms-client
  (:use #:cl
        #:alexandria
        #:lem)
  (:import-from #:lem-rooms-client/utils
                #:do-sequence
                #:hash
                #:pretty-json
                #:once)
  (:import-from #:lem-rooms-client/room
                #:room-management-pane
                #:room-room
                #:room-id
                #:room-name
                #:room-directory
                #:room-invitation
                #:room-owner-p
                #:register-room
                #:remove-room
                #:set-room-directory
                #:find-room-by-id
                #:find-room-by-file
                #:default-room)
  (:import-from #:lem-rooms-client/defcommand
                #:define-rooms-command
                #:rooms-command-name
                #:rooms-command-description
                #:list-rooms-commands)
  (:import-from #:lem-rooms-client/chase
                #:chase-client-id
                #:chase-on
                #:chase)
  (:import-from #:lem-rooms-client/editor
                #:lsp-to-lem-position
                #:position-of
                #:move-to-position*
                #:with-save-cursor
                #:close-rightside-window)
  (:local-nicknames (#:cursor #:lem-rooms-client/cursor)
                    (#:agent #:rooms-client/agent)
                    (#:agent-api #:rooms-client/agent-api)
                    (#:buffer #:lem-rooms-client/buffer)
                    (#:management-pane #:lem-rooms-client/management-pane)
                    (#:client #:rooms-client/client)
                    (#:connected-hook #:lem-rooms-client/connected-hook)
                    (#:sign-in #:lem-rooms-client/sign-in)
                    (#:room #:lem-rooms-client/room)))
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

(defvar *client* nil)

(defun client ()
  *client*)

(defun set-client (client)
  (setf *client* client))

(defclass client (client:client) ())

(defmethod (setf client:client-access-token) :before (token (client client))
  (setf (lem:config :rooms.access-token) token))

(defmethod (setf client:client-user) :before (user (client client))
  (setf (lem:config :room.user) user))

(defun launch-client ()
  (let ((client (or (client)
                    (client:launch :class-name 'client
                                   :access-token (lem:config :rooms.access-token)
                                   :user (lem:config :room.user)
                                   :on-message 'on-message
                                   :on-connected 'on-connected
                                   :on-disconnected 'on-disconnected
                                   :on-edit 'on-edit
                                   :on-users 'on-users
                                   :on-comments 'on-comments
                                   :on-messages 'on-messages
                                   :on-file-changed 'on-file-changed))))
    (set-client client)
    client))

(defun sign-out (client)
  (setf (lem:config :rooms.access-token) nil)
  (when client
    (client:sign-out client)))

(defmethod client:sign-in ((client client))
  (setf (client:client-access-token client)
        (sign-in:sign-in (client:client-agent client))))

(defun init ()
  (rooms-mode t)
  (init-editor-hooks)
  (let ((client (launch-client)))
    (client:sign-in-if-required client)))

(defun init-with-backdoor (name)
  (rooms-mode t)
  (init-editor-hooks)
  (let ((client (launch-client)))
    (client:sign-in-backdoor client name)))

(defun init-editor-hooks ()
  (add-hook *post-command-hook* 'on-post-command)
  (add-hook *editor-abort-hook* 'on-post-command)
  (add-hook *find-file-hook* 'on-find-file)
  (add-hook *exit-editor-hook*
            (lambda ()
              (agent:destroy-agent-if-alive
               (client:client-agent (client)))))
  (add-hook (variable-value 'before-change-functions :global t) 'on-before-change))

(defun get-ranges (point)
  (let ((buffer (point-buffer point))
        (ranges '()))
    (when (buffer-mark-p buffer)
      (let* ((mark (buffer-mark buffer))
             (start (position-of mark))
             (end (position-of point)))
        (push (hash :start (min start end)
                    :end (max start end)) ranges)))
    (when-let (ov (lem/isearch:get-current-highlight-overlay (point-buffer point)))
      (push (hash :start (position-of (overlay-start ov))
                  :end (position-of (overlay-end ov)))
            ranges))
    ranges))

(defun notify-focus (point)
  (let ((buffer (point-buffer point)))
    (cond ((buffer:room-id buffer)
           (agent-api:focus (client:client-agent (client))
                            :room-id (buffer:room-id buffer)
                            :path (buffer:path buffer)
                            :position (position-of point)
                            :ranges (get-ranges point)))
          ((default-room)
           (agent-api:focus (client:client-agent (client))
                            ;; TODO: 複数のroomを開いている場合にどうするか
                            :room-id (room-id (default-room))
                            :path nil
                            :position nil
                            :ranges nil)))))

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
         (agent-api:edit (client:client-agent (client))
                         :room-id room-id
                         :path (buffer:path buffer)
                         :ops (vector (hash :range (hash :start position
                                                         :end position)
                                            :text arg))))
        (integer
         (with-point ((end point))
           (unless (character-offset end arg)
             (buffer-end end))
           (agent-api:edit (client:client-agent (client))
                           :room-id room-id
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
                                            :comments (management-pane:convert-comments
                                                       (map 'list
                                                            #'agent-api::convert-to-comment
                                                            comments)))
                    (redraw-display))))))

(defun on-disconnected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (when-let* ((room (find-room-by-id room-id))
                              (pane (room-management-pane room)))
                    (management-pane:disconnected pane)
                    (connected-hook:disconnect)
                    (management-pane:redraw pane :users '())
                    (redraw-display))))))

(defun update-cursor (user)
  (when (agent-api:user-state-active user)
    (or (when-let (buffer (buffer:find-buffer-by-room-and-path
                           (agent-api:user-state-room-id user)
                           (agent-api:user-state-path user)))
          (when (eq buffer (window-buffer (current-window)))
            (cursor:update-cursor buffer
                                  (agent-api:user-state-client-id user)
                                  (agent-api:user-state-name user)
                                  (agent-api:user-state-color user)
                                  (lsp-to-lem-position (agent-api:user-state-position user))
                                  (when-let (ranges (agent-api:user-state-ranges user))
                                    ;; TODO: rangeが複数の場合にも対応する
                                    (let ((range (first ranges)))
                                      (list (lsp-to-lem-position (agent-api:range-start range))
                                            (lsp-to-lem-position (agent-api:range-end range))))))
            t))
        (cursor:delete-cursor (agent-api:user-state-client-id user)))))

(defun update-cursors (users)
  (let ((exclude-deleting-users '()))
    (do-sequence (user users)
      (declare (agent-api:user-state user))
      (unless (agent-api:user-state-myself user)
        (update-cursor user))
      (push (agent-api:user-state-client-id user)
            exclude-deleting-users))
    (cursor:delete-cursors-by-excluding-ids exclude-deleting-users)))

(defvar *users* nil)

(defun on-users (params)
  (send-event
   (lambda ()
     (when-let* ((users (map 'list
                             #'agent-api:convert-to-user-state
                             (remove-if #'null (gethash "users" params))))
                 (room-id (block found
                            (map ()
                                 (lambda (user)
                                   (return-from found
                                     (agent-api:user-state-room-id user)))
                                 users)))
                 (room (find-room-by-id room-id)))

       (setf *users* users)

       (update-cursors users)

       (when (chase-client-id)
         (when-let ((user (find (chase-client-id)
                                users
                                :key #'agent-api:user-state-client-id
                                :test #'equal)))
           (jump-to user room :chase t)))

       (when (zerop (event-queue-length))
         (management-pane:redraw (room-management-pane room) :users users)
         (redraw-display))))))

(defun on-comments (client event)
  (declare (ignore client))
  (send-event
   (lambda ()
     (when-let ((room (find-room-by-id (agent-api:commented-event-room-id event))))
       (management-pane:redraw
        (room-management-pane room)
        :comments (management-pane:convert-comments
                   (agent-api:commented-event-comments event)))))))

(defun on-messages (params)
  (declare (ignore params))
  ;; TODO
  )

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
                (text (agent-api:get-text (client:client-agent (client))
                                          :room-id (room-id room)
                                          :path (buffer:path buffer))))
      (unless (equal text (buffer-text buffer))
        (replace-buffer-text buffer text)
        (message "Rooms BUG/FIX: The text synchronization had failed, so it was resynchronized")))))

(defun open-file (client room path text)
  ;; TODO: room-ownerであるかの判別、room ownerが退室してから入室した場合を考える
  (if (room-owner-p room)
      (agent-api:share-file (client:client-agent client)
                            :room-id (room-id room)
                            :path path
                            :text text)
      (agent-api:sync-file (client:client-agent client)
                           :room-id (room-id room)
                           :path path
                           :text text)))

(defun on-find-file (buffer)
  (when-let (room (find-room-by-file (buffer-filename buffer)))
    (let* ((room-id (room-id room))
           (path (namestring
                  (enough-namestring (buffer-filename buffer)
                                     (room-directory room))))
           (text
             (open-file (client) room path (buffer-text buffer))))
      (when (and text (string/= text (buffer-text buffer)))
        (erase-buffer buffer)
        (insert-string (buffer-point buffer) text)
        (buffer-start (buffer-point buffer))
        (when (buffer-enable-undo-p buffer)
          (buffer-disable-undo buffer)
          (buffer-enable-undo buffer)))
      (buffer:register-room-id-and-path buffer room-id path)
      (once (start-timer (make-idle-timer 'check-buffer-sync) 2000 :repeat t)
            :name 'check-buffer-sync))))

(defun start-room (room)
  (management-pane:connected (room-management-pane room))
  (management-pane:redraw (room-management-pane room))
  (management-pane:open-management-pane room)
  (find-file (room-directory room)))

(defun enter-room (room &key then)
  (check-type room agent-api:room)
  (client:enter-room (client) room)
  (connected-hook:add (lambda ()
                        (funcall then))))

(defun prompt-for-scope (prompt)
  (prompt-for-string prompt
                     :initial-value "public"
                     :completion-function (lambda (s)
                                            (completion s '("public" "private")))
                     :test-function (lambda (s)
                                      (member s '("public" "private") :test #'equal))))

(defun get-web-url ()
  (or (uiop:getenv "ROOMS_WEB_URL")
      "http://localhost:5173"))

(defun show-room-url (room)
  (let* ((url (format nil "~A/rooms/~A" (get-web-url) (room:room-id room)))
         (window (display-popup-message (format nil "You can share this room by opening~2%    ~A    ~2%copied URL to clipboard " url)
                                        :timeout nil
                                        :style '(:gravity :center))))
    (copy-to-clipboard url)
    (unwind-protect (read-key)
      (delete-popup-message window))))

(define-rooms-command rooms-create-room () ()
  "Create a new room"
  (init)

  (let ((existing-room (default-room)))
    (when existing-room
      (unless (prompt-for-y-or-n-p "You are already in a room. Do you want to exit the room and create a new one?")
        (return-from rooms-create-room)))

    (let* ((room-name (prompt-for-string "Room name: "
                                         :test-function (lambda (string)
                                                          (< 0 (length string)))))
           (scope (prompt-for-scope "Room scope: "))
           (directory (prompt-for-directory "Share directory: "
                                            :existing t
                                            :directory (buffer-directory)))
           (room-json (client:create-room (client) :scope scope :name room-name))
           (room-id (agent-api:room-id room-json))
           (management-pane (management-pane:create-pane room-id)))

      (when existing-room
        (exit-room existing-room))

      (let ((room (register-room
                   :room room-json
                   :room-name (agent-api:room-name room-json)
                   :directory directory
                   :management-pane management-pane
                   :owner-p t)))
        (management-pane:connecting management-pane)
        (management-pane:redraw management-pane)
        (enter-room room-json
                    :then (lambda ()
                            (agent-api:share-directory (client:client-agent (client))
                                                       :room-id room-id
                                                       :path directory)
                            (start-room room)
                            (show-room-url room)))))))

(defun join-room (room-json)
  (let* ((room-id (agent-api:room-id room-json))
         (room (find-room-by-id room-id)))
    (cond (room
           (management-pane:open-management-pane room)
           (find-file (room-directory room)))
          (t
           (when-let (existing-room (default-room))
             (unless (prompt-for-y-or-n-p
                      "You are already in a room. Do you want to exit the room and join the new one?")
	       (return-from join-room))
             (exit-room existing-room))
           (let* ((management-pane (management-pane:create-pane room-id))
                  (room (register-room
                         :room room-json
                         :room-name (agent-api:room-name room-json)
                         :management-pane management-pane
                         :owner-p nil)))
             (management-pane:connecting management-pane)
             (management-pane:redraw management-pane)
             (enter-room room-json
                         :then (lambda ()
                                 (let ((directory
                                         (client:sync-directory (client) room-json)))
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
                  :items (client:get-rooms (client))
                  :select-callback (lambda (component room)
                                     (start-timer (make-idle-timer (lambda ()
                                                                     (join-room room)))
                                                  0)
                                     (lem/multi-column-list:quit component)))))

(defun exit-room (room)
  (agent-api:disconnect (client:client-agent (client)) :room-id (room-id room))
  (remove-room room))

(define-rooms-command rooms-exit-room () ()
  (when-let (room (get-current-room))
    (exit-room room)
    (management-pane:delete-management-pane room)))

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
                           (client:create-invitation (client)
                                                     (room-id room))
                           (room-invitation room)))
           (code (agent-api:invitation-code invitation)))
      (show-message (format nil " Invitation code: ~A ~2% copied to clipboard" code)
                    :style '(:gravity :center)
                    :timeout nil)
      (copy-to-clipboard code)
      (setf (room-invitation room) invitation))))

(define-rooms-command rooms-join-by-invitation-code (invitation-code) ((:string "Invitation code: "))
  "Enter the room where you received the invitation"
  (init)
  (let ((room-json (client:join-by-invitation-code (client) invitation-code)))
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
  (sign-out (client))
  (init)
  (message "Sign-in Successful"))

(define-command rooms-backdoor (name) ((:string "Name: "))
  (sign-out (client))
  (init-with-backdoor name)
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
             (client:comment (client) (room-room room) text))))
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
      (let* ((completion-items (mapcar (lambda (user)
                                         (lem/completion-mode:make-completion-item
                                          :label (agent-api:user-state-name user)))
                                       user-states))
             (user-name
               (prompt-for-string
                "To which user?: "
                :completion-function (lambda (s)
                                       (completion-strings
                                        s
                                        completion-items
                                        :key #'lem/completion-mode:completion-item-label))
                :test-function (lambda (s)
                                 (member s
                                         completion-items
                                         :key #'lem/completion-mode:completion-item-label
                                         :test #'equal)))))
        (find user-name
              user-states
              :key #'agent-api:user-state-name
              :test #'string=))))

(defun jump-to (user-state room &key chase)
  (let ((path (agent-api:user-state-path user-state))
        (position (agent-api:user-state-position user-state)))
    (unless path
      (return-from jump-to nil))
    (find-file (merge-pathnames path (room-directory room)))
    (move-to-position* (current-point) position)
    (when chase
      (chase (current-point) user-state))
    t))

(defun get-other-user-states ()
  (when-let (room (get-current-room))
    (values (remove-if #'agent-api:user-state-myself
                       (agent-api:get-users (client:client-agent (client))
                                            :room-id (room-id room)))
            room)))

(define-rooms-command rooms-jump-to-other-user-cursor () ()
  "Jump to the cursor position of other user"
  (multiple-value-bind (user-states room)
      (get-other-user-states)
    (when user-states
      (let ((user-state (choose-user user-states)))
        (unless (jump-to user-state room)
          (editor-error "The user has not opened any files"))))))

(define-rooms-command rooms-chase-other-user-cursor () ()
  "chase the cursor position of other user"
  (multiple-value-bind (user-states room)
      (get-other-user-states)
    (when user-states
      (let ((user-state (choose-user user-states)))
        (chase-on :user-state user-state :room room :client (client))))))

(defun prompt-for-rooms-command ()
  (let* ((commands (list-rooms-commands))
         (completions (loop :for command :in commands
                            :collect (lem/completion-mode:make-completion-item
                                      :label (rooms-command-name command)
                                      :detail (rooms-command-description command))))
         (lem-core::*default-prompt-gravity* :top-display)
         (lem/prompt-window::*fill-width* t)
         (*prompt-after-activate-hook* *prompt-after-activate-hook*))
    (add-hook *prompt-after-activate-hook* 'lem/prompt-window::prompt-completion)
    (find-command
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
      :syntax-table lem-lisp-syntax:*syntax-table*))))

(define-command rooms-command-palette (arg) (:universal-nil)
  (let ((command (prompt-for-rooms-command)))
    (call-command command arg)))


;;;;


(defun directory-mode/insert-user-name (point item)
  (declare (type lem/directory-mode/internal::item item))
  (when-let* ((buffer (point-buffer point))
              (room (find-room-by-file (buffer-directory buffer))))
    (let ((path (enough-namestring (lem/directory-mode/internal::item-pathname item)
                                   (room-directory room))))
      (dolist (user *users*)
        (when-let (user-path (agent-api:user-state-path user))
          (when (and (not (equal ".." (lem/directory-mode/internal::item-content item)))
                     (starts-with-subseq path user-path))
            (insert-string point " ")
            (management-pane::insert-color-text
             point
             (format nil " ~A " (agent-api:user-state-name user))
             (agent-api:user-state-color user))
            (return)))))))

(unless (member 'directory-mode/insert-user-name
                lem/directory-mode/internal::*file-entry-inserters*)
  (appendf lem/directory-mode/internal::*file-entry-inserters*
           (list 'directory-mode/insert-user-name)))
