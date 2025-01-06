(asdf:load-asd (probe-file "rooms-client.asd"))

(defsystem "lem-rooms-client"
  :depends-on ("rooms-client")
  :serial t
  :components ((:file "utils")
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
