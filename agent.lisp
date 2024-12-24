(uiop:define-package #:lem-rooms-client/agent
  (:use #:cl
        #:lem-rooms-client/utils)
  (:export #:agent-alive-p
           #:run-agent
           #:destroy-agent-if-alive
           #:notify
           #:call))
(in-package #:lem-rooms-client/agent)

(defparameter *agent-path* (asdf:system-relative-pathname :lem-rooms-client "sdk/agent.js"))

(defvar *agent* nil)

(defstruct agent
  jsonrpc
  process)

(defun to-simple-character-array (string)
  (make-array (length string) :initial-contents string :element-type 'character))

(defun run-process ()
  (async-process:create-process
   (list "node"
         (namestring (probe-file *agent-path*)))))

(defun agent-alive-p (&optional (agent *agent*))
  (and agent (async-process:process-alive-p (agent-process agent))))

(defun run-agent (&key on-message
                       on-connected
                       on-disconnected
                       on-edit
                       on-users
                       on-comments
                       on-file-changed)
  (assert (not (agent-alive-p *agent*)))
  (let* ((process (run-process))
         (jsonrpc (jsonrpc:make-client))
         (stream (lem-lsp-mode/async-process-stream:make-input-stream
                  process
                  :logger (lambda (output)
                            (log:info "agent output: ~A" output)))))
    (jsonrpc:expose jsonrpc "message" on-message)
    (jsonrpc:expose jsonrpc "connected" on-connected)
    (jsonrpc:expose jsonrpc "disconnected" on-disconnected)
    (jsonrpc:expose jsonrpc "edit" on-edit)
    (jsonrpc:expose jsonrpc "users" on-users)
    (jsonrpc:expose jsonrpc "comments" on-comments)
    (jsonrpc:expose jsonrpc "fileChanged" on-file-changed)
    (jsonrpc/client:client-connect-using-class
     jsonrpc
     'lem-lsp-mode/lem-stdio-transport:lem-stdio-transport
     :process process
     :stream stream)
    (setf *agent*
          (make-agent :jsonrpc jsonrpc
                      :process process))))

(defun destroy-agent-if-alive (&key (agent *agent*))
  (when (agent-alive-p agent)
    (async-process:delete-process (agent-process agent))))

(defun notify (method params &key (agent *agent*))
  (check-type agent agent)
  (log:info "Notify ~A ~A" method (pretty-json params))
  (jsonrpc:notify (agent-jsonrpc agent) method params))

(defun call (method params &key (agent *agent*))
  (check-type agent agent)
  (log:info "Call ~A ~A" method (pretty-json params))
  (let ((response (jsonrpc:call (agent-jsonrpc agent) method params)))
    (log:info "Call response ~A" (pretty-json response))
    response))
