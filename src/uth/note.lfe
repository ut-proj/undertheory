(defmodule uth.note
  (export all))

(defun whole () 1)
(defun half () 0.5)
(defun quarter () 0.25)
(defun eighth () 0.125)
(defun sixteenth () 0.0625)
(defun thirty-second () 0.03125)
(defun sixty-fourth () 0.015625)

;; Dotted notes

(defun dotted-half ()
  (+ (half) (quarter)))

(defun dotted-quarter ()
  (+ (quarter) (eighth)))

(defun dotted-eighth ()
  (+ (eighth) (sixteenth)))

(defun dotted-sixteenth ()
  (+ (sixteenth) (thirty-second)))

(defun dotted-thirty-second ()
  (+ (thirty-second) (sixty-fourth)))

;; Aliases

(defun n () (whole))
(defun w () (n))
(defun n/2 () (half))
(defun n/4 () (quarter))
(defun n/8 () (eighth))
(defun n/16 () (sixteenth))
(defun n/32 () (thirty-second))
(defun n/64 () (sixty-fourth))
(defun n/2. () (dotted-half))
(defun n/4. () (dotted-quarter))
(defun n/8. () (dotted-eighth))
(defun n/16. () (dotted-sixteenth))
(defun n/32. () (dotted-thirty-second))

(defun denom-lookup
  ((1) (whole))
  ((2) (half))
  ((4) (quarter))
  ((8) (eighth))
  ((16) (sixteenth))
  ((32) (thirty-second))
  ((64) (sixty-fourth)))
   
(defun duration-fn
  ((bpm (= `#(,notes/meas ,note-type) _time-sig))
   (lambda ()
     (* 1000 (/ bpm 60)))))
            
