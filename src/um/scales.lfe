(defmodule um.scale
  (export all))

(defun chromatic ()
  '(1 b2 2 b3 3 4 b5 5 b6 6 b7 7))

(defun major ()
  '(1 2 3 4 5 6 7))

(defun ionian ()
  (major))

(defun dorian ()
  '(1 2 b3 4 5 6 b7))

(defun phrygian ()
  '(1 b2 b3 4 5 b6 b7))

(defun phrygian-dominant ()
  '(1 b2 3 4 5 b6 b7))

(defun gypsy-scale ()
  (phrygian-dominant))

(defun lydian ()
  '(1 2 3 |#4| 5 6 7))

(defun lydian-dominant ()
  '(1 2 3 |#4| 5 6 b7))

(defun acoustic ()
  (lydian-dominant))

(defun mixolydian ()
  '(1 2 3 4 5 6 b7))

(defun natural-minor ()
  '(1 2 b3 4 5 b6 b7))

(defun major-pentatonic ()
  '(1 2 3 5 6))

(defun minor-pentatonic ()
  '(1 2 3 5 6))

(defun ->pitch
    ((1) 0)
    (('b2) 1)
    ((2) 2)
    (('|#2|) 3)
    (('b3) 3)
    ((3) 4)
    (('|#3|) 5)
    ((''b4) 4)
    ((4) 5)
    (('|#4|) 6)
    (('b5) 6)
    ((5) 7)
    (('|#5|) 8)
    (('b6) 8)
    ((6) 9)
    (('|#6|) 10)
    (('b7) 10)
    ((7) 11)
    (('|#7|) 12)
    (('b8) 11)
    ((8) 12)
    (('|#8|) 13)
    (('b9) 13)
    ((9) 14)
    (('|#9|) 15)
    (('b10) 15)
    ((10) 16)
    (('|#10|) 17)
    (('b11) 17)
    ((11) 18)
    (('|#11|) 19)
    (('b12) 19)
    ((12) 20)
    (('|#12|) 21)
    (('b13) 21)
    ((13) 22)
    (('|#13|) 23))

(defun midi (scale)
  (list-comp ((<- s scale))
    (->pitch s)))
