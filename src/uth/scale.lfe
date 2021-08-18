(defmodule uth.scale
  (export all))

(defun chromatic ()
  '(1 b2 2 b3 3 4 b5 5 b6 6 b7 7))

;;; Diatonic Scales

(defun ionian ()
  '(1 2 3 4 5 6 7))

(defun major ()
  (ionian))

(defun dorian ()
  '(1 2 b3 4 5 6 b7))

(defun phrygian ()
  '(1 b2 b3 4 5 b6 b7))

(defun phrygian-dominant ()
  '(1 b2 3 4 5 b6 b7))

(defun gypsy ()
  (phrygian-dominant))

(defun lydian ()
  '(1 2 3 |#4| 5 6 7))

(defun lydian-dominant ()
  '(1 2 3 |#4| 5 6 b7))

(defun acoustic ()
  (lydian-dominant))

(defun mixolydian ()
  '(1 2 3 4 5 6 b7))

(defun aeolian ()
  '(1 2 b3 4 5 b6 b7))

(defun natural-minor ()
  (aeolian))

(defun harmonic-minor ()
  '(1 2 b3 4 5 b6 7))

(defun melodic-minor-asc ()
  '(1 2 b3 4 5 6 7))

(defun melodic-minor-desc ()
  '(1 2 b3 4 5 b6 b7))

(defun locrian ()
  '(1 b2 b3 4 b5 b6 b7))

;;; Pentatonic Scales

(defun major-pentatonic ()
  '(1 2 3 5 6))

(defun blues-major ()
  '(1 2 4 5 6))

(defun minor-pentatonic ()
  '(1 b3 4 5 b7))

(defun blues-minor ()
  '(1 b3 4 b6 b7))

(defun man-gong ()
  (blues-minor))

(defun relative-minor-pentatonic ()
  '(1 3 4 5 7))

(defun japanese ()
  '(1 2 4 5 6))

(defun suspended-egyptian ()
  '(1 2 4 5 b7))

;;; Hexatonic Scales

(defun whole-tone ()
  '(1 2 3 |#4| |#5| |#6|))

(defun symmetrical-augmented ()
  '(1 b3 3 5 |#5| 7))

(defun prometheus ()
  '(1 2 3 |#4| 6 b7))

(defun tritone ()
  '(1 b2 3 b5 5 b7))
