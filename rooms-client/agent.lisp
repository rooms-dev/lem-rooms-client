(uiop:define-package #:rooms-client/agent
  (:use #:cl)
  (:export #:agent-alive-p
           #:run-agent
           #:destroy-agent-if-alive
           #:notify
           #:call))
(in-package #:rooms-client/agent)

(defparameter *agent-path* (asdf:system-relative-pathname :rooms-client "sdk/agent.js"))

(defvar *agent* nil)

(defstruct agent
  jsonrpc
  process)

(defun installed-nodejs-p ()
  (zerop
   (nth-value 2
              (uiop:run-program '("which" "node")
                                :output :string
                                :ignore-error-status t))))

(defun run-process ()
  (unless (installed-nodejs-p)
    (error "Node.js is not installed"))
  (setf (uiop:getenv "NODE_NO_WARNINGS") "1")
  (async-process:create-process
   (list "node"
         (namestring (probe-file *agent-path*)))))

(defun agent-alive-p (agent)
  (and agent (async-process:process-alive-p (agent-process agent))))

(defun run-agent (&key on-message
                       on-connected
                       on-disconnected
                       on-edit
                       on-users
                       on-comments
                       on-messages
                       on-file-changed)
  (let* ((process (run-process))
         (jsonrpc (jsonrpc:make-client))
         (stream (rooms-client/async-process-stream:make-input-stream
                  process
                  :logger (lambda (output)
                            (log:info "agent output: ~A" output)))))
    (jsonrpc:expose jsonrpc "message" on-message)
    (jsonrpc:expose jsonrpc "connected" on-connected)
    (jsonrpc:expose jsonrpc "disconnected" on-disconnected)
    (jsonrpc:expose jsonrpc "edit" on-edit)
    (jsonrpc:expose jsonrpc "users" on-users)
    (jsonrpc:expose jsonrpc "comments" on-comments)
    (jsonrpc:expose jsonrpc "userMessage" on-messages)
    (jsonrpc:expose jsonrpc "fileChanged" on-file-changed)
    (jsonrpc/client:client-connect-using-class
     jsonrpc
     'rooms-client/stdio-transport:stdio-transport
     :process process
     :stream stream)
    (make-agent :jsonrpc jsonrpc
                :process process)))

(defun destroy-agent-if-alive (agent)
  (when (agent-alive-p agent)
    (async-process:delete-process (agent-process agent))))

(defun notify (agent method params)
  (check-type agent agent)
  (log:debug "Notify ~A ~A" method (pretty-json params))
  (jsonrpc:notify (agent-jsonrpc agent) method params))

(defun call (agent method params)
  (check-type agent agent)
  (log:debug "Call ~A ~A" method (pretty-json params))
  (let ((response (jsonrpc:call (agent-jsonrpc agent) method params)))
    (log:debug "Call response ~A" (pretty-json response))
    response))

;;; utils

(defun pretty-json (object)
  (yason:with-output-to-string* (:indent t)
    (yason:encode object)))
