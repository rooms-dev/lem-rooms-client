(uiop:define-package :lem-rooms-client/sign-in
  (:use #:cl
        #:lem
        #:alexandria
        #:lem-rooms-client/utils
        #:lem-rooms-client/config)
  (:local-nicknames (:rooms-api :lem-rooms-client/rooms-api))
  (:export #:rooms-sign-in))
(in-package :lem-rooms-client/sign-in)

(defun authorize-url ()
  (let ((json (rooms-api:get "/github/authorize-url")))
    (gethash "url" json)))

(defun authenticate (code)
  (rooms-api:get (format nil "/github/authenticate?code=~A" code)))

(defun open-authorize-url-with-browser-frontend (authorize-url)
  (js-eval (current-window)
           (format nil "let x = document.createElement('a');~@
                        x.href = '~A';~@
                        x.target = '_blank';~@
                        document.body.appendChild(x);~@
                        x.click();~@
                        document.body.removeChild(x);"
                   authorize-url)))

(defun prompt-code-with-browser-frontend ()
  (js-eval (current-window) "prompt('code: ')" :wait t))

(defun sign-in-with-browser-frontend ()
  (let ((authorize-url (authorize-url)))
    (open-authorize-url-with-browser-frontend authorize-url)
    (when-let ((code (prompt-for-string "code: ")))
      (setf (access-token)
            (gethash "access_token" (authenticate code)))
      (message "Sign-in Successful"))))

(defun sign-in ()
  (let ((authorize-url (authorize-url)))
    (open-external-file authorize-url)
    (when-let ((code (prompt-for-string (format nil "~% ~A ~%~%code: " authorize-url))))
      (setf (access-token)
            (gethash "access_token" (authenticate code)))
      (message "Sign-in Successful"))))

(define-command rooms-sign-in () ()
  (if (browser-frontend-p)
      (sign-in-with-browser-frontend)
      (sign-in)))

(define-command rooms-backdoor (name) ((:string "Name: "))
  (let ((response (rooms-api:backdoor name)))
    (setf (access-token)
          (gethash "access_token" response))
    (message "Sign-in Successful")))
