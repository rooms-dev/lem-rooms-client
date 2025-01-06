(defsystem "lem-rooms-client"
  :depends-on ()
  :serial t
  :components ((:module "rooms-client"
                :serial t
                :components ((:file "agent")
                             (:file "agent-api")))
               (:file "utils")
               (:file "defcommand")
               (:file "editor")
               (:file "cursor")
               (:file "sign-in")
               (:file "user")
               (:file "room")
               (:file "buffer")
               (:file "connected-hook")
               (:file "api-client")
               (:file "management-pane")
               (:file "main")))
