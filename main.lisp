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
                    (#:client #:lem-rooms-client/client)))
(in-package #:lem-rooms-client)

(defvar *inhibit-change-notification* nil)

(defvar *connected-hooks* '())
(defvar *connected* nil)

(defun notify-focus (point)
  (let ((buffer (point-buffer point)))
    (when (buffer:room-id buffer)
      (agent-api:focus :name (config:user-name)
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

(defun on-message (params)
  (message "~A" (gethash "message" params)))

(defun on-connected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (dolist (hook *connected-hooks*)
                    (funcall hook))
                  (setf *connected-hooks* '())
                  (setf *connected* t)
                  (let ((buffer (room-management-buffer (find-room-by-id room-id))))
                    (management-buffer:update buffer :status :connected))
                  (redraw-display)))))

(defun on-disconnected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (setf *connected* nil)
                  (let ((buffer (room-management-buffer (find-room-by-id room-id))))
                    (management-buffer:update buffer :status :disconnected))
                  (redraw-display)))))

(defun add-connected-hook (hook)
  (if *connected*
      (funcall hook)
      (push hook *connected-hooks*)))

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
         (notify-focus (current-point))
         (redraw-display))))))

(defun update-cursors (room users)
  (let ((client-id (room-client-id room)))
    (do-sequence (user users)
      (let ((id (gethash "id" user))
            (name (gethash "name" user))
            (color (gethash "color" user))
            (room-id (gethash "roomId" user))
            (path (gethash "path" user))
            (position (gethash "position" user)))
        (unless (equal client-id id)
          (when-let (buffer (buffer:find-buffer-by-room-and-path room-id path))
            (when (eq buffer (window-buffer (current-window)))
              (cursor:set-cursor buffer id name color (lsp-to-lem-position position)))))))))

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
       (update-room-users room
                          (map 'list
                               #'convert-user
                               (remove-if-not (lambda (user)
                                                (equal room-id (gethash "roomId" user)))
                                              users)))
       (update-cursors room users)
       (management-buffer:update (room-management-buffer room)
                                 :users users
                                 :status (if *connected* :connected :disconnected))
       (redraw-display)))))

(defun on-comments (params)
  (message "~A" (pretty-json params)))

(defun on-post-command ()
  (notify-focus (current-point)))

(defun on-find-file (buffer)
  (when-let (room (find-room-by-file (buffer-filename buffer)))
    (let ((room-id (room-id room))
          (path (namestring
                 (enough-namestring (buffer-filename buffer)
                                    (room-directory room)))))
      (let ((text (agent-api:open-file :room-id room-id
                                       :path path
                                       :text (buffer-text buffer))))
        (when (and text (string/= text (buffer-text buffer)))
          (erase-buffer buffer)
          (insert-string (buffer-point buffer) text)
          (buffer-start (buffer-point buffer))
          (when (buffer-enable-undo-p buffer)
            (buffer-disable-undo buffer)
            (buffer-enable-undo buffer))))
      (buffer:register-room-id-and-path buffer room-id path))))

(defun init ()
  (client:init)
  (add-hook *post-command-hook* 'on-post-command)
  (add-hook *find-file-hook* 'on-find-file)
  (add-hook *exit-editor-hook* (lambda () (destroy-agent-if-alive)))
  (add-hook (variable-value 'before-change-functions :global t) 'on-before-change))

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
    (add-connected-hook (lambda ()
                          (funcall then client-id)))))

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
         (room (rooms-api:create-room :name room-name :scope scope)))
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
  (init)
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
                  :items (rooms-api:get-rooms)
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
                           (rooms-api:create-invitation (room-id room))
                           (room-invitation room)))
           (code (gethash "code" invitation)))
      (show-message (format nil " Invitation code: ~A ~2% copied to clipboard" code)
                    :style '(:gravity :center)
                    :timeout nil)
      (copy-to-clipboard code)
      (setf (room-invitation room) invitation))))

(define-command rooms-join-by-invitation-code (invitation-code) ((:string "Invitation code: "))
  (init)
  (let ((room-json (rooms-api:get-room-by-invitation invitation-code)))
    (join-room room-json)))
