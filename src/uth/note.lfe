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
