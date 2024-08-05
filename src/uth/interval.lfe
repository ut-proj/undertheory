;;;; This module is not MIDI-aware; as such, all inputs need to be converted to
;;;; values in the twleve tone scale (either one or two octaves).
(defmodule uth.interval
  (export all))

(defun names ()
  '(P0 m2 M2 m3 M3 P4 a4 dim5 P5 m6 M6 m7 M7
    P8 m9 M9 m10 M10 P11 a11 dim12 P12 m13 M13 m14 M14
    P15))

(defun numbers ()
  '(0 1 2 3 4 5 6 6 7 8 9 10 11
    12 13 14 15 16 17 18 18 19 20 21 22 23
    24))

(defun num->name (number)
  (mref (maps:from_list (lists:zip (numbers) (names))) number))

(defun name->num (name)
  (mref (maps:from_list (lists:zip (names) (numbers))) name))

(defun name (note1 note2)
  (name note1 note2 1))

(defun norm (note-num1 note-num2)
  (cond
   ((< note-num1 note-num2) (list note-num1 note-num2))
   ('true (norm note-num1 (+ note-num2 12)))))

(defun name
  ((note1 note2 octaves) (when (andalso (is_atom note1) (is_atom note2)))
   (name (uth.note:number note1) (uth.note:number note2) octaves))
  ((num1 num2 octaves)
   (let* ((`(,n1 ,n2) (norm num1 num2))
          (raw (- n2 n1))
          (intv (cond
                 ((and (== octaves 2) (== raw 24)) 24)
                 ((== octaves 2) (rem raw 24))
                 ((== raw 12) 12)
                 ('true (rem raw 12)))))
     (num->name intv))))
