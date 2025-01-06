(defsystem "rooms-client"
  :depends-on ()
  :serial t
  :pathname "rooms-client"
  :components ((:file "agent")
               (:file "agent-api")))
