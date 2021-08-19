(defmodule uth.scale
  (export all))

(defun all ()
  #m(
     chromatic (1 b2 2 b3 3 4 b5 5 b6 6 b7 7)
     ;; Diatonic
     ionian     (1  2  3   4  5  6  7)
     dorian     (1  2 b3   4  5  6 b7)
     phrygian   (1 b2 b3   4  5 b6 b7)
     lydian     (1  2  3 |#4| 5  6  7)
     mixolydian (1  2  3   4  5  6 b7)
     aeolian    (1  2 b3   4  5 b6 b7)
     locrian    (1 b2 b3   4 b5 b6 b7)
     ;; Pentatonic
     major-pentatonic              (1  2 3  5  6)
     blues-major-pentatonic        (1  2 4  5  6)
     minor-pentatonic              (1 b3 4  5 b7)
     blues-minor-pentatonic        (1 b3 4 b6 b7)
     relative-minor-pentatonic     (1  3 4  5  7)
     suspended-egyptian-pentatonic (1  2 4  5 b7)
     ;; Bebop
     ;; Hexatonic
     whole-tone            (1  2 3 |#4| |#5| |#6|)
     symmetrical-augmented (1 b3 3   5  |#5|   7)
     prometheus            (1  2 3 |#4|   6   b7)
     tritone               (1 b2 3  b5    5   b7)
     ;; Diminished/octatonic
     ;; Persian
     ;; Messiaen
     ;; Other
     ))

(defun chromatic ()
  (mref (all) 'chromatic))

;;; Diatonic Scales

(defun diatonic ()
  (let ((keys '(ionian
                dorian
                phrygian
                lydian
                mixolydian
                aeolian
                locrian)))
    (maps:with keys (all))))

(defun diatonic (type)
  (mref (all) type))

(defun diatonic
  (('major 'I) (mref (all) 'ionian))
  (('major 'ii) (mref (all) 'dorian))
  (('major 'iii) (mref (all) 'phrygian))
  (('major 'IV) (mref (all) 'lydian))
  (('major 'V) (mref (all) 'mixolydian))
  (('major 'vi) (mref (all) 'aeolian))
  (('major 'viio) (mref (all) 'locrian)))

(defun ionian ()
  (mref (all) 'ionian))

(defun major ()
  (mref (all) 'ionian))

(defun dorian ()
  (mref (all) 'dorian))

(defun phrygian ()
  (mref (all) 'phrygian))

(defun lydian ()
  (mref (all) 'lydian))

(defun mixolydian ()
  (mref (all) 'mixolydian))

(defun aeolian ()
  (mref (all) 'aeolian))

(defun locrian ()
  (mref (all) 'locrian))

;;; Melodic minor

(defun phrygian-dominant ()
  '(1 b2 3 4 5 b6 b7))

(defun gypsy ()
  (phrygian-dominant))

(defun lydian-dominant ()
  '(1 2 3 |#4| 5 6 b7))

(defun acoustic ()
  (lydian-dominant))

(defun natural-minor ()
  (aeolian))

(defun harmonic-minor ()
  '(1 2 b3 4 5 b6 7))

(defun melodic-minor-asc ()
  '(1 2 b3 4 5 6 7))

(defun melodic-minor-desc ()
  '(1 2 b3 4 5 b6 b7))

;;; Pentatonic Scales

(defun pentatonic (type)
  (mref (all) (list_to_atom (++ (atom_to_list type) "-pentatonic"))))

(defun major-pentatonic ()
  (mref (all) 'major-pentatonic))

(defun blues-major ()
  (mref (all) 'blues-major-pentatonic))

(defun minor-pentatonic ()
  (mref (all) 'minor-pentatonic))

(defun blues-minor ()
  (mref (all) 'blues-minor-pentatonic))

(defun man-gong ()
  (mref (all) 'blues-minor-pentatonic))

(defun relative-minor-pentatonic ()
  (mref (all) 'relative-minor-pentatonic))

(defun japanese ()
  (mref (all) 'blues-major-pentatonic))

(defun suspended-egyptian ()
  (mref (all) 'suspended-egyptian-pentatonic))

;;; Bebop

;;; Hexatonic Scales

(defun whole-tone ()
  (mref (all) 'whole-tone))

(defun symmetrical-augmented ()
  (mref (all) 'symmetrical-augmented))

(defun prometheus ()
  (mref (all) 'prometheus))

(defun tritone ()
  (mref (all) 'tritone))

;;; Diminished/octatonic

;;; Persian

;;; Messiaen

;;; Other