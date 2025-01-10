(defsystem "rooms-bot"
  :depends-on ("rooms-client"
               "ollama")
  :components ((:file "main")))
