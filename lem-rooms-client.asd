(defsystem "lem-rooms-client"
  :depends-on ()
  :serial t
  :components ((:file "utils")
               (:file "editor")
               (:file "config")
               (:file "cursor")
               (:file "agent")
               (:File "agent-api")
               (:file "sign-in")
               (:file "user")
               (:file "room")
               (:file "buffer")
               (:file "connected-hook")
               (:file "api-client")
               (:file "management-pane")
               (:file "main")))
