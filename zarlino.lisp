(in-package #:cl-user)

(defpackage #:zarlino
  (:use #:cl+qt)
  (:export #:main))

(in-package #:zarlino)
(in-readtable :qtools)

(define-widget main-window (QWidget) ())

(define-subwidget (main-window note-on-button)
    (q+:make-qpushbutton "Note on" main-window))

(define-subwidget (main-window notes-off-button)
    (q+:make-qpushbutton "Notes off" main-window))

(define-subwidget (main-window temperament-list)
    (q+:make-qlistwidget main-window)
  (dolist (temperament '("just" "pythagorean" "equal"))
    (q+:add-item temperament-list temperament)))

(define-subwidget (main-window instrument-label) (q+:make-qlabel "Instrument"))

(define-subwidget (main-window instrument-input) (q+:make-qspinbox)
  (q+:set-range instrument-input 1 128)
  (q+:set-value instrument-input 1))

(define-subwidget (main-window a4-label) (q+:make-qlabel "A4 frequency"))

(define-subwidget (main-window a4-input) (q+:make-qspinbox)
  (q+:set-range a4-input 221 779)
  (q+:set-value a4-input 440))

(define-subwidget (main-window main-layout) (q+:make-qvboxlayout)
  (q+:add-widget main-layout temperament-list)
  (q+:add-widget main-layout instrument-label)
  (q+:add-widget main-layout instrument-input)
  (q+:add-widget main-layout a4-label)
  (q+:add-widget main-layout a4-input)
  (q+:add-widget main-layout note-on-button)
  (q+:add-widget main-layout notes-off-button)
  (q+:set-layout main-window main-layout))

(define-signal (main-window note-on) ())
(define-signal (main-window notes-off) ())
(define-signal (main-window set-temperament) ())
(define-signal (main-window set-instrument) ())
(define-signal (main-window set-a4) ())

(define-slot (main-window note-on) ()
  (declare (connected note-on-button (pressed)))
  (zarlino.synth:note-on (+ (random 80) 40)))

(define-slot (main-window notes-off) ()
  (declare (connected notes-off-button (pressed)))
  (zarlino.synth:notes-off))

(define-slot (main-window set-temperament) ()
  (declare (connected temperament-list (current-text-changed string)))
  (flet ((make-keyword (str)
           (values (intern (string-upcase str) "KEYWORD"))))
    (zarlino.synth:set-tuning
     :temperament (make-keyword (q+:text (q+:current-item temperament-list))))))

(define-slot (main-window set-instrument) ()
  (declare (connected instrument-input (value-changed int)))
  (zarlino.synth:set-instrument (q+:value instrument-input)))

(define-slot (main-window set-a4) ()
  (declare (connected a4-input (value-changed int)))
  (zarlino.synth:set-tuning :a4-freq (q+:value a4-input)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-subwidget (main-window main-layout) (q+:make-qgridlayout main-window)
;;   (q+:add-widget main-layout temperament-list 0 0 2 1)
;;   (q+:add-widget main-layout a4-label 0 1)
;;   (q+:add-widget main-layout a4-input 0 2)
;;   (q+:add-widget main-layout note-on-button 1 1)
;;   (q+:add-widget main-layout notes-off-button 1 2))
