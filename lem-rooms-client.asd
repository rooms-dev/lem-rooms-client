(defsystem "lem-rooms-client"
  :depends-on ()
  :serial t
  :components ((:file "utils")
               (:file "config")
               (:file "rooms-api")
               (:file "cursor")
               (:file "agent")
               (:File "agent-api")
               (:file "sign-in")
               (:file "room")
               (:file "buffer")
               (:file "main")))
