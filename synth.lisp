(in-package #:zarlino.synth)

(defconstant +num-midi-notes+ 128)

(defvar *settings* (fluidsynth:new-settings '(("synth.polyphony" 128)
                                              ("audio.driver" "pulseaudio"))))

(defvar *synth* (fluidsynth:new-synth *settings*))

(let ((sf-path "/usr/share/sounds/sf2/FluidR3_GM.sf2"))
  (if (probe-file sf-path)
      (fluidsynth:sfload *synth* sf-path 1)))

(defvar *audio-driver* (fluidsynth:new-audio-driver *settings* *synth*))

(defvar *instrument* 0)

(defun note-on (midi-key &optional (volume 127) (channel 0))
  (zerop (fluidsynth:noteon *synth* channel midi-key volume)))

(defun notes-on (midi-keys &optional (volume 127) (channel 0))
  (null (loop for k in midi-keys do (note-on k volume channel))))

(defun note-off (midi-key &optional (channel 0))
  (zerop (fluidsynth:noteoff *synth* channel midi-key)))

(defun notes-off (&optional (channel 0))
  (zerop (fluidsynth:all-notes-off *synth* channel)))

(defun get-instrument ()
  *instrument*)

(defun set-instrument (instrument-number)
  (fluidsynth:program-change *synth* 0 instrument-number)
  (setq *instrument* instrument-number))

;; tuning

(defconstant +just-intervals+
  '(1 16/15 9/8 6/5 5/4 4/3 65/45 3/2 8/5 5/3 16/9 15/8 2))

(defconstant +pythagorean-intervals+
  '(1 256/243 9/8 32/27 81/64 4/3 729/512 3/2 128/81 27/16 16/9 243/128 2))

(defconstant +equal-intervals+
  (loop for i from 0 to 12 collect (expt 2 (/ i 12))))

(defun interval (cents)
  (expt 10 (/ cents (/ 1200 (log 2 10)))))

(defun cents (interval)
  (* 1200 (/ (log interval 10) (log 2 10))))

(defun cent-difference (freq-1 freq-2)
  (* 1200 (log (/ freq-2 freq-1) 2)))

(defun freq (root-freq half-steps intervals)
  (let ((interval (nth (mod half-steps 12) intervals))
        (octave (floor half-steps 12)))
    (* root-freq interval (expt 2 octave))))

(defun midi-freq (midi-key &optional (intervals +equal-intervals+)
                             (a4-freq 440))
  (let ((d4-freq (* a4-freq (/ 1 (nth 7 intervals))))
        (distance-from-d4 (- midi-key 62)))
    (freq d4-freq distance-from-d4 intervals)))

(defun cents-from-midi-key-0 (midi-key intervals
                              &optional (a4-freq 440))
  (let ((midi-key-0-freq (midi-freq 0 +equal-intervals+ 440)))
    (cent-difference midi-key-0-freq (midi-freq midi-key intervals a4-freq))))

(defun make-tuning (intervals &optional (a4-freq 440))
  (loop for k below +num-midi-notes+
        collect (coerce (cents-from-midi-key-0 k intervals a4-freq)
                        'double-float)))

(defun tune-notes (name tuning)
  (cffi:with-foreign-string (name name)
    (cffi:with-foreign-object (pitches :double +num-midi-notes+)
      (dotimes (i +num-midi-notes+)
        (setf (cffi:mem-aref pitches :double i) (nth i tuning)))
      (fluidsynth:create-key-tuning *synth* 0 0 name pitches) ;deprecated?
      (fluidsynth:activate-tuning *synth* 0 0 0 1))))

(defun get-tuning ()
  (cffi:with-foreign-objects ((name :char 16)
                              (pitches :double +num-midi-notes+))
    (fluidsynth:tuning-dump *synth* 0 0 name +num-midi-notes+ pitches)
    (loop for i below +num-midi-notes+
          collect (cffi:mem-aref pitches :double i))))

(defun set-tuning (temperament &optional (a4-freq 440))
  (case temperament
    (:equal (tune-notes "equal" (make-tuning +equal-intervals+ a4-freq)))
    (:just (tune-notes "just" (make-tuning +just-intervals+ a4-freq)))
    (:pythagorean (tune-notes "pythagorean"
                              (make-tuning +pythagorean-intervals+ a4-freq)))))
