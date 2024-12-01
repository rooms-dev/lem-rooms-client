(uiop:define-package #:lem-rooms-client
  (:use #:cl
        #:alexandria
        #:lem
        #:lem-rooms-client/utils
        #:lem-rooms-client/agent
        #:lem-rooms-client/room)
  (:local-nicknames (#:cursor #:lem-rooms-client/cursor)
                    (#:config #:lem-rooms-client/config)
                    (#:rooms-api #:lem-rooms-client/rooms-api)
                    (#:agent-api #:lem-rooms-client/agent-api)
                    (#:sign-in #:lem-rooms-client/sign-in)
                    (#:buffer #:lem-rooms-client/buffer)))
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

(defun on-post-command ()
  (notify-focus (current-point)))

(defun on-before-change (point arg)
  (unless *inhibit-change-notification*
    (let* ((buffer (point-buffer point))
           (position (position-of point))
           (room-id (buffer:room-id buffer)))
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
                    (update-rooms-buffer buffer :status :connected))))))

(defun on-disconnected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (setf *connected* nil)
                  (let ((buffer (room-management-buffer (find-room-by-id room-id))))
                    (update-rooms-buffer buffer :status :disconnected))))))

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
       (update-cursors room users)
       (update-rooms-buffer (room-management-buffer room)
                            :users users
                            :status (if *connected* :connected :disconnected))
       (redraw-display)))))

(defun ensure-sign-in ()
  (unless (config:access-token)
    (sign-in:rooms-sign-in)))

(defun setup-agent (websocket-url)
  (destroy-agent-if-alive)
  (run-agent :websocket-url websocket-url
             :access-token (config:access-token)
             :on-message 'on-message
             :on-connected 'on-connected
             :on-disconnected 'on-disconnected
             :on-edit 'on-edit
             :on-users 'on-users))

(defun ensure-user ()
  (unless (config:user)
    (let ((user (rooms-api:get-user)))
      (setf (config:user)
            (list :id (rooms-api:user-id user)
                  :github-login (rooms-api:user-github-login user)
                  :avatar-url (rooms-api:user-avatar-url user))))))

(defun init ()
  (ensure-sign-in)
  (ensure-user)
  (add-hook *post-command-hook* 'on-post-command)
  (add-hook *find-file-hook* 'on-find-file)
  (add-hook *exit-editor-hook* (lambda ()
                                 (destroy-agent-if-alive))))

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
      (buffer:register-room-id-and-path buffer room-id path)
      (add-hook (variable-value 'before-change-functions :buffer buffer) 'on-before-change))))

(defun prompt-for-scope (prompt)
  (prompt-for-string prompt
                     :initial-value "public"
                     :completion-function (lambda (s)
                                            (completion s '("public" "private")))
                     :test-function (lambda (s)
                                      (member s '("public" "private") :test #'equal))))

(defun update-rooms-buffer (buffer &key users status)
  (erase-buffer buffer)
  (with-point ((point (buffer-point buffer) :left-inserting))
    (insert-string point
                   "Rooms"
                   :attribute (make-attribute
                               :bold t
                               :foreground (best-foreground-color (background-color))))
    (insert-character point #\newline)
    (insert-character point #\newline)
    (insert-string point (format nil "Status:~%"))
    (ecase status
      (:connecting
       (insert-string point "Connecting..." :attribute (make-attribute :foreground "orange")))
      (:connected
       (insert-string point "Connected" :attribute (make-attribute :foreground "green")))
      (:disconnected
       (insert-string point "Disconnected" :attribute (make-attribute :foreground "red"))))
    (insert-character point #\newline)
    (insert-string point (format nil "~%Users:~%"))
    (do-sequence (user users)
      (let ((name (gethash "name" user))
            (color (gethash "color" user)))
        (insert-string point
                       (format nil " ~A " name)
                       :attribute (make-attribute :foreground (best-foreground-color color)
                                                  :background color))
        (insert-character point #\newline)))))

(defun create-rooms-pane ()
  (let ((buffer (make-buffer "*Rooms right-side-pane*" :temporary t :enable-undo-p nil)))
    (setf (not-switchable-buffer-p buffer) t)
    (update-rooms-buffer buffer :status :connecting)
    (make-rightside-window buffer :width 30)
    buffer))

(defun start-room (client-id room-id directory management-buffer)
  (update-rooms-buffer management-buffer :status :connected)
  (register-directory :room-id room-id
                      :client-id client-id
                      :directory directory
                      :management-buffer management-buffer)
  (find-file directory))

(defun enter-room (room-id &key then)
  (let* ((enter-room-result (agent-api:enter-room :room-id room-id :user-name (config:user-name)))
         (client-id (gethash "clientID" enter-room-result)))
    (add-connected-hook (lambda ()
                          (funcall then client-id)))))

(define-command rooms-create-room () ()
  (init)
  (let* ((room-name (prompt-for-string "Room name: " :test-function (lambda (string) (< 0 (length string)))))
         (scope (prompt-for-scope "Room scope: "))
         (directory (prompt-for-directory "Share directory: "
                                          :existing t
                                          :directory (buffer-directory)))
         (room (rooms-api:create-room :name room-name :scope scope)))
    (setup-agent (rooms-api:room-websocket-url room))
    (let ((room-id (rooms-api:room-id room))
          (management-buffer (create-rooms-pane)))
      (enter-room room-id
                  :then (lambda (client-id)
                          (agent-api:share-directory :room-id room-id :path directory)
                          (start-room client-id room-id directory management-buffer))))))

(defun join-room (room)
  (let ((room-id (rooms-api:room-id room))
        (management-buffer (create-rooms-pane)))
    (setup-agent (rooms-api:room-websocket-url room))
    (enter-room room-id
                :then (lambda (client-id)
                        (let ((directory (agent-api:sync-directory :room-id room-id)))
                          (start-room client-id
                                      room-id
                                      (namestring
                                       (uiop:ensure-directory-pathname directory))
                                      management-buffer))))))

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
