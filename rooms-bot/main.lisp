(uiop:define-package #:rooms-bot
  (:use #:cl)
  (:local-nicknames (#:client #:rooms-client/client)
                    (#:agent-api #:rooms-client/agent-api)))
(in-package #:rooms-bot)

(defun launch ()
  (let ((client (client:launch :on-message 'on-message
                               :on-connected 'on-connected
                               :on-disconnected 'on-disconnected
                               :on-edit 'on-edit

                               :on-users 'on-users
                               :on-comments 'on-comments
                               :on-file-changed 'on-file-changed)))
    (client:sign-in-backdoor client "James")
    client))

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
  (dolist (comment (agent-api:commented-event-added event))
    (unless (equal (agent-api:commented-user-id (agent-api:comment-user comment))
                   (client:user-id client))
      (sleep 1)
      (client:comment client
                      (agent-api:commented-event-room-id event)
                      (agent-api:comment-text comment))))
  #+(or)
  (let ((comment (first (agent-api:commented-event-added event))))
    ;; TODO: 自分と他人を区別できるようにする
    (when (equal "cxxxr"
                 (agent-api:commented-user-name
                  (agent-api:comment-user comment)))
      (let ((response
              (ollama/utils:slurp
               (ollama:generate (agent-api:comment-text
                                 comment)
                                :model "phi4"))))
        (client:comment client $room response)))))

(defun on-file-changed (params)
  (log:info "on-file-changed: ~A" (pretty-json params)))

(defun pretty-json (object)
  (yason:with-output-to-string* (:indent t)
    (yason:encode object)))

(defun test ()
  (defparameter $ (launch))
  (defparameter $room (first (client:get-rooms $)))
  (client:enter-room $ $room))

(eval-when ()
  (defparameter $ (launch))
  (defparameter $room (first (client:get-rooms $)))
  (client:enter-room $ $room)
  (client:sync-directory $ $room)
  (client:get-comments $ $room)
  (client:comment $ $room "hello"))

#|

# Bot

## コメントをしたらコメントを返してくれるBot
- 必要なAPI
  - on-comments
  - client:comment

## SBCL Bot
- コメントに書いた式を評価してくれる
- scratchバッファ
  - SBCL Bot用のscratchバッファを用意して、そこに式を書き込んだら、評価して結果を挿入してくれる　
- Room内のプロジェクト全体をquickloadしてくれる
  - そこでのログ出力は専用のファイルに挿入していってほしい
- AI
  - コードのリファクタリングの提案
  - 実装したい機能を聞いたら提案してくれる
|#
