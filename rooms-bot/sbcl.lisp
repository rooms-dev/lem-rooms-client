(uiop:define-package #:rooms-bot/sbcl
  (:use #:cl)
  (:local-nicknames (#:client #:rooms-client/client)
                    (#:agent-api #:rooms-client/agent-api)))
(in-package #:rooms-bot/sbcl)

(defvar *client*)

(defun launch ()
  (let ((client (client:launch :on-message 'on-message
                               :on-connected 'on-connected
                               :on-disconnected 'on-disconnected
                               :on-edit 'on-edit
                               :on-users 'on-users
                               :on-comments 'on-comments
                               :on-file-changed 'on-file-changed)))
    (client:sign-in-backdoor client "James")
    (setf *client* client)
    client))

(defun enter-room (client)
  (let ((room (first (client:get-rooms client))))
    (client:enter-room client room)
    (client:sync-directory client room)))

(defun on-message (params)
  (log:info "on-message: ~A" (pretty-json params)))

(defun on-connected (params)
  (log:info "on-connected: ~A" (pretty-json params)))

(defun on-disconnected (params)
  (log:info "on-disconnected: ~A" (pretty-json params)))

(defun on-edit (params)
  (log:info "on-edit: ~A" (pretty-json params)))

(defun on-users (params)
  (declare (ignorable params))
  ;; (log:info "on-users: ~A" (pretty-json params))
  )

(defun on-comments (client event)
  (handler-bind ((error (lambda (c)
                          (log:info "~A" c))))
    (dolist (comment (agent-api:commented-event-added event))
      (unless (equal (agent-api:commented-user-id (agent-api:comment-user comment))
                     (client:user-id client))
        (let ((text (agent-api:comment-text comment)))
          (ppcre:register-groups-bind (form-string) ("^/eval\\s+(.*)" text)
            (when form-string
              (log:info form-string)
              (handler-case (values (read-from-string form-string))
                (error (condition)
                  (log:info "~A" condition)
                  (client:comment client
                                  (agent-api:commented-event-room-id event)
                                  (format nil "Error: ~A" condition)))
                (:no-error (form)
                  (log:info form)
                  (handler-case (values (eval form))
                    (error (condition)
                      (client:comment client
                                      (agent-api:commented-event-room-id event)
                                      (format nil "Error: ~A" condition)))
                    (:no-error (result)
                      (let ((response (handler-case (prin1-to-string result)
                                        (error (c)
                                          (princ-to-string c)))))
                        (client:comment client
                                        (agent-api:commented-event-room-id event)
                                        (format nil "~A" response))))))))))))))

(defun on-file-changed (params)
  (log:info "on-file-changed: ~A" (pretty-json params)))

(defun pretty-json (object)
  (yason:with-output-to-string* (:indent t)
    (yason:encode object)))


#|
TODO:
- [X] コメントに"/eval expr"とすると、exprを評価し結果をコメントで返す
- [ ] 専用の共有バッファにsbclの出力を書き込んでいく
- [ ] プロジェクトの初回読み込み
- [ ] ファイルを編集したら自動でcompile-file
- [ ] コンパイルエラーがあればその箇所に赤線を引く
|#
