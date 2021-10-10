(defmodule uth.chord
  (export all))

(defun all ()
  #m(;; Major
     major    (1 3 5)
     major6   (1 3 5 6)
     major7   (1 3 5 7)
     major9   (1 2 5 9)
     major6/9 (1 3 5 6 9)
     major11  (1 3 5 11)
     major#11 (1 3 5 |#11|)
     majorb13 (1 3 5 b13)
     major13  (1 3 5 13)
     ;; Dominant/Seventh
     dom7    (1 3 5 b7)
     dom7b9  (1 3 5 b7   b9)
     dom9    (1 3 5 b7    9)
     dom7#9  (1 3 5 b7  |#9|)
     dom11   (1 3 5 b7   11)
     dom13   (1 3 5 b7   13)
     dom7#11 (1 3 5 b7 |#11|)
     dom7#13 (1 3 5 b7 |#13|)     
     ;; Suspended Dominant/Seventh
     sus2  (1 2 5)
     sus4  (1 4 5)
     7sus4 (1 4 5 b7)
     b8sus (1 4 5 b9)
     ;; Minor
     minor (1 b3 5)
     min7  (1 b3 5 b7)
     minmaj7 (1 b3 5 7)
     m6      (1 b3 5 6)
     m9      (1 b3 5 9)
     m11     (1 b3 5 11)
     m13     (1 b3 b 13)
     ;; Minor Diminished
     dim  (1 b3 b5)
     dim7 (1 b3 b5 bb7)
     m7b5 (1 b3 b5 b7)
     ;; Augmented
     aug (1 3 |#5|)
     7#b (1 3 |#5| 7)
     ;; 6th add6
     ;; maj#4 maj#11 lydian
     ;; 7th dom
     ;; half-dim m7b5
     ))

(defun name (name)
  (mref (all) name))

;;; Chord Inversions

(defun invert
  ((`(,head . ,tail))
   (lists:append tail (list head))))

(defun invert (chrd nth)
  (invert chrd nth 0))

(defun invert
  ((chrd nth _) (when (< nth 2))
   chrd)
  ((chrd nth count) (when (== nth count))
   chrd)
  ((chrd nth count)
   (invert (invert chrd) nth (+ 1 count))))

(defun invert-a (chrd)
  chrd)

(defun invert-b (chrd)
  (invert chrd 1))

(defun invert-c (chrd)
  (invert chrd 2))

;;; Modes

(defun modes ()
  #m(ionian #m(I (1 3 5)
               ii (2 4 6)
               iii (3 5 7)
               IV (4 6 8)
               V (5 7 9)
               vi (6 8 10)
               viio (7 9 11))
     ;; TODO
     dorian #m()
     phyrgian #m()
     lydian #m()
     mixolydian #m()
     aeolian #m(i (6 8 10)
                iio (7 9 11)
                bIII (8 10 12)
                iv (9 11 13)
                v (10 12 14)
                bVI (11 13 15)
                bVII (12 14 16))
     ;; TODO
     locrian #m()))

(defun mode (mode chord)
  (mref (mref (modes) mode) chord))

(defun I () (mode 'ionian 'I))
(defun ii () (mode 'ionian 'ii))
(defun iii () (mode 'ionian 'iii))
(defun IV () (mode 'ionian 'IV))
(defun V () (mode 'ionian 'V))
(defun vi () (mode 'ionian 'vi))
(defun viio () (mode 'ionian 'viio))

(defun i () (mode 'aeolian 'i))
(defun iio () (mode 'aeolian 'iio))
(defun bIII () (mode 'aeolian 'bIII))
(defun iv () (mode 'aeolian 'iv))
(defun v () (mode 'aeolian 'v))
(defun bVI () (mode 'aeolian 'bVI))
(defun bVII () (mode 'aeolian 'bVII))

;;; Utility functions

(defun add
  ((notes note) (when (is_integer note))
   (lists:append notes (list note)))
  ((notes more-notes) (when (is_list more-notes))
   (lists:append notes more-notes)))
