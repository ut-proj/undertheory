(defmodule uth.note
  (export all))

(defun timings()
  `#m(1 1.0
      1/2 0.5
      1/4 0.25
      1/8 0.125
      1/16 0.0625
      1/32 0.03125
      1/64 0.015625
      ;; dotted notes
      1/2. ,(+ 0.5 0.25)
      1/4. ,(+ 0.25 0.125)
      1/8. ,(+ 0.125 0.0625)
      1/16. ,(+ 0.0625 0.03125)
      1/32. ,(+ 0.03125 0.015625)
      1/64. ,(+ 0.015625 0.0078125)))

(defun denom->timing
  ((1) (mref (timings) '1))
  ((2) (mref (timings) '1/2))
  ((4) (mref (timings) '1/4))
  ((8) (mref (timings) '1/8))
  ((16) (mref (timings) '1/16))
  ((32) (mref (timings) '1/32))
  ((64) (mref (timings) '1/64)))

(defun duration-fn
  ((bpm (= `#(,_beats/meas ,beat-note) _time-sig))
   (lambda (k)
     (let ((beat (denom->timing beat-note)))
     (trunc (* 1000
               (/ bpm 60)
               beat
               (/ (mref (timings) k) beat)))))))

(defun 1oct () 12)
(defun 2oct () 24)

(defun mod-1oct (note) (rem note (1oct)))
(defun mod-2oct (note) (rem note (2oct)))

(defun names ()
  "This includes enharmonics."
  `(C C# Db D D# Eb E F F# Gb G G# Ab A A# Bb B
      ,@(enharmonic-sharps)
      ,@(enharmonic-flats)
      ,@(double-flats)
      ,@(double-sharps)))

(defun sharps () '(C# D# F# G# A#))
(defun enharmonic-sharps () '(B# E#))
(defun double-sharps () '(C## D## E## F## G## A## B##))
(defun flats () '(Db Eb Gb Ab Bb))
(defun enharmonic-flats () '(Fb Cb))
(defun double-flats () '(Cbb Dbb Ebb Fbb Gbb Abb Bbb))

(defun !flats () (++ (sharps)
                     (enharmonic-sharps)
                     (double-sharps)
                     (enharmonic-flats)
                     (double-flats)))

(defun !sharps () (++ (enharmonic-sharps)
                      (double-sharps)
                      (flats)
                      (enharmonic-flats)
                      (double-flats)))

(defun flat? (note) (not (lists:member note (!flats))))
(defun sharp? (note) (not (lists:member note (!sharps))))
(defun accidental (note)
  (cond
   ((flat? note) 'flat)
   ((sharp? note) 'sharp)
   ('true 'natural)))

(defun numbers ()
  "Useful for working with intervals; not related to MIDI in any way."
  (list 0 1 1 2 3 3 4 5 6 6 7 8 8 9 10 10 11
        0 5 ; enharmonic sharps
        4 11 ; enharmonic flats
        10 0 2 3 5 7 9 ; double-flats
        2 4 6 7 9 11 1   ; double-sharps
        ))

(defun name->number () (lists:zip (names) (numbers)))
(defun number->name () (lists:zip (numbers) (names)))

(defun name-all (number) (proplists:get_all_values number (number->name)))

(defun name
  ((number '#(flat))
   (car
    (lists:filter
     #'flat?/1
     (name-all number))))
  ((number '#(sharp))
   (car
    (lists:filter
     #'sharp?/1
     (name-all number))))
  ((number '#(all))
   (name-all number))
  ((number _)
   (car (name-all number))))

(defun name (number)
  (car (name-all number)))

(defun number (name)
  (proplists:get_value name (name->number)))

(defun number
  ((name 'with-error)
   (if (lists:member name (names))
     (number name)
     (uth.errors:note-name))))

(defun ->intervals
  ((`(,previous . ()))
   #(error "list of notes too short"))
  ((`(,previous . ,notes))
   (->intervals previous notes '())))

(defun ->intervals
  ((previous '() acc)
   (lists:reverse acc))
  ((previous `(,head . ,tail) acc)
   (->intervals head tail (cons (- (number head) (number previous)) acc))))

(defun retrograde ()
  'not-implemented)

(defun invert (root-note notes)
  (invert root-note notes #(one)))

(defun invert (root-note notes opts)
  ;; TODO: check for error(s):
  (let ((ivals (->intervals notes)))
    (uth.interval:->notes root-note
                          (uth.interval:invert ivals)
                          opts)))
