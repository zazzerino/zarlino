(in-package #:cl-user)

(defpackage #:zarlino.gui
  (:use #:cl+qt)
  (:export #:main))

(in-package #:zarlino.gui)
(in-readtable :qtools)

;; main window

(define-widget main-window (QMainWindow) ())

;; temperament

(define-subwidget (main-window temperament-list)
    (q+:make-qlistwidget main-window)
  (dolist (temperament '("just" "pythagorean" "equal"))
    (q+:add-item temperament-list temperament)))

(define-slot (main-window set-temperament) ()
  (declare (connected temperament-list (current-text-changed string)))
  (flet ((make-keyword (str)
           (values (intern (string-upcase str) "KEYWORD"))))
    (zarlino.synth:set-tuning
     :temperament (make-keyword (q+:text (q+:current-item temperament-list))))))

;; instrument

(define-subwidget (main-window instrument-label) (q+:make-qlabel "Instrument:"))

(define-subwidget (main-window instrument-input) (q+:make-qspinbox)
  (q+:set-range instrument-input 1 128)
  (q+:set-value instrument-input 1))

(define-slot (main-window set-instrument) ()
  (declare (connected instrument-input (value-changed int)))
  (zarlino.synth:set-instrument (q+:value instrument-input)))

;; a4 frequency

(define-subwidget (main-window a4-label) (q+:make-qlabel "A4 frequency:"))

(define-subwidget (main-window a4-input) (q+:make-qspinbox)
  (q+:set-range a4-input 221 779)
  (q+:set-value a4-input 440))

(define-slot (main-window set-a4) ()
  (declare (connected a4-input (value-changed int)))
  (zarlino.synth:set-tuning :a4-freq (q+:value a4-input)))

;; midi files

(define-subwidget (main-window midi-file-label)
    (q+:make-qlabel "No midi file selected."))

(define-subwidget (main-window open-midi-file-button)
    (q+:make-qpushbutton "Open midi file" main-window))

(define-slot (main-window open-midi-file) ()
  (declare (connected open-midi-file-button (pressed)))
  (let ((file-name (q+:qfiledialog-get-open-file-name
		    main-window "Open midi file" "resources/midi"
		    "Midi Files (*.mid *.midi)")))
    (zarlino.synth:load-midi-file file-name)
    (setf (q+:text midi-file-label) (file-namestring file-name))))

;; midi player control

(define-subwidget (main-window play-button)
    (q+:make-qpushbutton "Play" main-window))

(define-slot (main-window play) ()
  (declare (connected play-button (pressed)))
  (zarlino.synth:play))

(define-subwidget (main-window pause-button)
    (q+:make-qpushbutton "Pause" main-window))

(define-slot (main-window pause) ()
  (declare (connected pause-button (pressed)))
  (zarlino.synth:pause)
  (zarlino.synth:notes-off))

(define-subwidget (main-window stop-button)
    (q+:make-qpushbutton "Stop" main-window))

(define-slot (main-window stop) ()
  (declare (connected stop-button (pressed)))
  (zarlino.synth:stop)
  (zarlino.synth:notes-off))

;; layout

(define-subwidget (main-window main-layout) (q+:make-qgridlayout)
  (q+:add-widget main-layout temperament-list 0 0 2 1)
  (q+:add-widget main-layout instrument-label 0 1)
  (q+:add-widget main-layout instrument-input 0 2)
  (q+:add-widget main-layout a4-label 1 1)
  (q+:add-widget main-layout a4-input 1 2)
  (q+:add-widget main-layout midi-file-label 0 3)
  (q+:add-widget main-layout open-midi-file-button 1 3)
  (q+:add-widget main-layout play-button 2 1)
  (q+:add-widget main-layout pause-button 2 2)
  (q+:add-widget main-layout stop-button 2 3)
  (let ((widget (q+:make-qwidget main-window)))
    (q+:set-window-title main-window "Zarlino")
    (q+:set-layout widget main-layout)
    (q+:set-central-widget main-window widget)))

;; (define-subwidget (main-window main-layout) (q+:make-qvboxlayout)
;;   (q+:add-widget main-layout temperament-list)
;;   (q+:add-widget main-layout instrument-label)
;;   (q+:add-widget main-layout instrument-input)
;;   (q+:add-widget main-layout a4-label)
;;   (q+:add-widget main-layout a4-input)
;;   (q+:add-widget main-layout midi-file-label)
;;   (q+:add-widget main-layout open-midi-file-button)
;;   (q+:add-widget main-layout play-button)
;;   (q+:add-widget main-layout pause-button)
;;   (q+:add-widget main-layout stop-button)
;;   (let ((widget (q+:make-qwidget main-window)))
;;     (q+:set-window-title main-window "Zarlino")
;;     (setf (q+:layout widget) main-layout)
;;     (setf (q+:central-widget main-window) widget)))
