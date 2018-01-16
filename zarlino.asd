;;;; zarlino.asd

(asdf:defsystem #:zarlino
  :description "Describe zarlino here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:qtools
               #:qtcore
               #:qtgui
               #:fluidsynth)
  :serial t
  :components ((:file "synth")
               (:file "gui")))
