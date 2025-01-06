(defsystem "rooms-client"
  :depends-on ("log4cl"
               "async-process"
               "jsonrpc"
               "cl-change-case")
  :serial t
  :pathname "rooms-client"
  :components ((:file "async-process-stream")
               (:file "stdio-transport")
               (:file "agent")
               (:file "agent-api")))
