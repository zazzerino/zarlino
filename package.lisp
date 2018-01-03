;;;; package.lisp

(defpackage #:zarlino.synth
  (:use #:cl)
  (:export #:note-on
           #:notes-on
           #:note-off
           #:notes-off
           #:get-instrument
           #:set-instrument
           #:get-tuning
           #:set-tuning))

(defpackage #:zarlino
  (:use #:cl+qt #:zarlino.synth)
  (:export #:main))
