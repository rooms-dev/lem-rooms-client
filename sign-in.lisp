(uiop:define-package :lem-rooms-client/sign-in
  (:use #:cl
        #:lem
        #:alexandria
        #:lem-rooms-client/config)
  (:local-nicknames (:rooms-api :lem-rooms-client/rooms-api))
  (:export #:rooms-sign-in
           #:sign-in-if-not-set-access-token))
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
  (let ((authorize-url (rooms-api:get-authorize-url)))
    (open-authorize-url-with-browser-frontend authorize-url)
    (when-let ((code (prompt-for-string "code: ")))
      (setf (access-token)
            (rooms-api:authenticated-access-token (rooms-api:authenticate code)))
      (message "Sign-in Successful"))))

(defun sign-in ()
  (let ((authorize-url (rooms-api:get-authorize-url)))
    (open-external-file authorize-url)
    (when-let ((code (prompt-for-string (format nil "~% ~A ~%~%code: " authorize-url))))
      (setf (access-token)
            (rooms-api:authenticated-access-token (rooms-api:authenticate code)))
      (message "Sign-in Successful"))))

(define-command rooms-sign-in () ()
  (if (lem-rooms-client/editor:browser-frontend-p)
      (sign-in-with-browser-frontend)
      (sign-in)))

(define-command rooms-backdoor (name) ((:string "Name: "))
  (let ((response (rooms-api:backdoor name)))
    (setf (access-token)
          (gethash "access_token" response))
    (message "Sign-in Successful")))

(defun sign-in-if-not-set-access-token ()
  (unless (access-token)
    (rooms-sign-in))
  (access-token))
