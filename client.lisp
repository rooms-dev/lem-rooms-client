(uiop:define-package #:lem-rooms-client/client
  (:use #:cl
        #:alexandria
        #:lem
        #:lem-rooms-client/utils
        #:lem-rooms-client/editor
        #:lem-rooms-client/agent
        #:lem-rooms-client/room
        #:lem-rooms-client/user)
  (:local-nicknames (#:agent #:lem-rooms-client/agent)
                    (#:cursor #:lem-rooms-client/cursor)
                    (#:config #:lem-rooms-client/config)
                    (#:rooms-api #:lem-rooms-client/rooms-api)
                    (#:agent-api #:lem-rooms-client/agent-api)
                    (#:sign-in #:lem-rooms-client/sign-in)
                    (#:buffer #:lem-rooms-client/buffer)
                    (#:management-buffer #:lem-rooms-client/management-buffer)
                    (#:connected-hook #:lem-rooms-client/connected-hook))
  (:export #:init
           #:notify-focus))
(in-package #:lem-rooms-client/client)

(defvar *inhibit-change-notification* nil)

(defun init ()
  (let ((access-token (sign-in-if-not-set-access-token)))
    (run-agent-if-not-alive access-token)
    (set-user-if-not-set access-token))
  
  (add-hook *post-command-hook* 'on-post-command)
  (add-hook *find-file-hook* 'on-find-file)
  (add-hook *exit-editor-hook* (lambda () (agent:destroy-agent-if-alive)))
  (add-hook (variable-value 'before-change-functions :global t) 'on-before-change))

(defun sign-in-if-not-set-access-token ()
  (unless (config:access-token)
    (sign-in:rooms-sign-in))
  (config:access-token))

(defun run-agent-if-not-alive (access-token)
  (unless (agent:agent-alive-p)
    (agent:run-agent :access-token access-token
                     :on-message 'on-message
                     :on-connected 'on-connected
                     :on-disconnected 'on-disconnected
                     :on-edit 'on-edit
                     :on-users 'on-users
                     :on-comments 'on-comments)))

(defun set-user-if-not-set (access-token)
  (unless (config:user)
    (let ((user (rooms-api:get-user access-token)))
      (setf (config:user)
            (list :id (rooms-api:user-id user)
                  :github-login (rooms-api:user-github-login user)
                  :avatar-url (rooms-api:user-avatar-url user))))))

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

(defun on-message (params)
  (message "~A" (gethash "message" params)))

(defun on-connected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (connected-hook:on-connect)
                  (let ((buffer (room-management-buffer (find-room-by-id room-id))))
                    (management-buffer:update buffer :status :connected))
                  (redraw-display)))))

(defun on-disconnected (params)
  (let ((room-id (gethash "roomId" params)))
    (send-event (lambda ()
                  (connected-hook:disconnect)
                  (let ((buffer (room-management-buffer (find-room-by-id room-id))))
                    (management-buffer:update buffer :status :disconnected))
                  (redraw-display)))))

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
                                 :status (if (connected-hook:connected-p) :connected :disconnected))
       (redraw-display)))))

(defun on-comments (params)
  (send-event
   (lambda ()
     (message "~A" (pretty-json params)))))

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
