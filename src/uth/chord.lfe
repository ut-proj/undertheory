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
     7#b (1 3 |#5| 7)))


;; 6th add6
;; maj#4 maj#11 lydian

;; 7th dom

;; half-dim m7b5