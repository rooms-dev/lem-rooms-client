(uiop:define-package :lem-rooms-client/sign-in
  (:use #:cl
        #:lem
        #:alexandria)
  (:local-nicknames (:agent-api :rooms-client/agent-api))
  (:export #:sign-in))
(in-package :lem-rooms-client/sign-in)

(defun open-authorize-url-with-browser-frontend (url)
  (js-eval (current-window)
           (format nil "let x = document.createElement('a');~@
                        x.href = '~A';~@
                        x.target = '_blank';~@
                        document.body.appendChild(x);~@
                        x.click();~@
                        document.body.removeChild(x);"
                   url)))

(defun prompt-code-with-browser-frontend ()
  (js-eval (current-window) "prompt('code: ')" :wait t))

(defun sign-in-with-browser-frontend ()
  (let ((authorize-url (agent-api:get-github-authorize-url)))
    (open-authorize-url-with-browser-frontend authorize-url)
    (when-let ((code (prompt-for-string "code: ")))
      (gethash "access_token" (agent-api:authenticate code)))))

(defun sign-in-default ()
  (let ((authorize-url (agent-api:get-github-authorize-url)))
    (ignore-errors (open-external-file authorize-url))
    (when-let ((code (prompt-for-string (format nil "~% ~A ~%~%code: " authorize-url))))
      (gethash "access_token" (agent-api:authenticate code)))))

(defun sign-in ()
  (if (lem-rooms-client/editor:browser-frontend-p)
      (sign-in-with-browser-frontend)
      (sign-in-default)))
