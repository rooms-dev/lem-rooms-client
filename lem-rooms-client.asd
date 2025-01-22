(asdf:load-asd (probe-file "rooms-client.asd"))

(defsystem "lem-rooms-client"
  :depends-on ("rooms-client")
  :serial t
  :components ((:file "utils")
               (:file "defcommand")
               (:file "editor")
               (:file "cursor")
               (:file "sign-in")
               (:file "room")
               (:file "buffer")
               (:file "connected-hook")
               (:file "management-pane")
               (:file "chase")
               (:file "main")))
