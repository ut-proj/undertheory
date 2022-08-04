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
     ;; Natural Minor
     ;;natural-minor
     ;; Melodic Minor
     melodic-minor-ascending (1  2 b3   4    5  6  7)
     phrygidorian            (1 b2 b3   4    5  6 b7)
     lydian-augmented        (1  2  3 |#4| |#5| 6  7)
     lydian-dominant         (1  2  3 |#4|   5  6 b7)
     myxaeolian              (1  2  3   4    5 b6 b7)
     aeolocrian              (1  2 b3   4   b5 b6 b7)
     super-locrian           (1 b2 b3  b4   b5 b6 b7)
     ;; Harmonic Minor
     harmonic-minor     (1   2 b3   4   5 b6   7)
     locrian-#6         (1  b2 b3   4  b5  6  b7)
     ionian-#5          (1   2  3   4 |#5| 6   7)
     ukrainian-dorian   (1   2 b3 |#4|  5  6  b7)
     phrygian-dominant  (1  b2  3   4   5 b6  b7)
     lydian-#2          (1 |#2| 3 |#4|  5  6   7)
     altered-diminished (1  b2 b3  b4  b5 b6 bb7)
     ;; Harmonic Major
     harmonic-major (1 2 3 4 5 b6 7)
     ;; Double Harmonic
     double-harmonic (1 b2  3   4  5 b6 7)
     hungarian-minor (1  2 b3 |#4| 5 b6 7)
     ;; Pentatonic
     major-pentatonic              (1  2 3  5  6)
     blues-major-pentatonic        (1  2 4  5  6)
     minor-pentatonic              (1 b3 4  5 b7)
     blues-minor-pentatonic        (1 b3 4 b6 b7)
     relative-minor-pentatonic     (1  3 4  5  7)
     suspended-egyptian-pentatonic (1  2 4  5 b7)
     ;; Bebop
     bebop-dominant       (1 2  3 4 5   6 b7  7)
     bebop-dorian         (1 2 b3 3 4   5  6 b7)
     bebop-major          (1 2  3 4 5 |#5| 6  7)
     bebop-melodic-minor  (1 2 b3 4 5 |#5| 6  7)
     bebop-harmonic-minor (1 2 b3 4 5  b6  b7 7)
     ;; Hexatonic
     whole-tone            (1  2 3 |#4| |#5| |#6|)
     symmetrical-augmented (1 b3 3   5  |#5|   7)
     prometheus            (1  2 3 |#4|   6   b7)
     tritone               (1 b2 3  b5    5   b7)
     ;; Diminished/octatonic
     c#-diminished (|#1| |#2| 3 |#4|  5    6 |#6| |#7|)
     d-diminished  (  1    2  3   4 |#4| |#5|  6  |#7|)
     eb-diminished ( b1    2 b3  b4   4    5   6    7)
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

;;; Natural Minor Scales

(defun natural-minor ()
  (aeolian))

;;; Melodic Minor Scales

(defun melodic-minor-asc ()
  (mref (all) 'melodic-minor-ascending))

(defun athenian ()
  (melodic-minor-asc))

(defun jazz-minor ()
  (melodic-minor-asc))

(defun phrygidorian ()
  (mref (all) 'phrygidorian))

(defun cappadocian ()
  (phrygidorian))

(defun assyrian ()
  (phrygidorian))

(defun lydian-augmented ()
  (mref (all) 'lydian-augmented))

(defun asgardian ()
  (lydian-augmented))

(defun lydian-dominant ()
  (mref (all) 'lydian-dominant))

(defun acoustic ()
  (lydian-dominant))

(defun overtone ()
  (lydian-dominant))

(defun lydomyxian ()
  (lydian-dominant))

(defun pontikonisian ()
  (lydian-dominant))

(defun myxaeolian ()
  (mref (all) 'myxaeolian))

(defun melodic-major ()
  (myxaeolian))

(defun hindu ()
  (myxaeolian))

(defun aeolian-dominant ()
  (myxaeolian))

(defun olympian ()
  (myxaeolian))

(defun aeolocrian ()
  (mref (all) 'aeolocrian))

(defun half-diminished ()
  (aeolocrian))

(defun sisyphean ()
  (aeolocrian))

(defun super-locrian ()
  (mref (all) 'super-locrian))

(defun altered-dominant ()
  (super-locrian))

(defun palamidian ()
  (super-locrian))

(defun melodic-minor-desc ()
  (mref (all) 'aeolian))

;;; Harmonic Minor Scales

(defun harmonic-minor ()
  (mref (all) 'harmonic-minor))

(defun locrian-#6 ()
  (mref (all) 'locrian-#6))

(defun ionian-#5 ()
  (mref (all) 'ionian-#5))

(defun ukrainian-dorian ()
  (mref (all) 'ukrainian-dorian))

(defun phrygian-dominant ()
  (mref (all) 'phrygian-dominant))

(defun gypsy ()
  (phrygian-dominant))

(defun lydian-#2 ()
  (mref (all) 'lydian-#2))

(defun altered-diminished ()
  (mref (all) 'altered-diminished))

;;; Harmonic Major, etc.

(defun harmonic-major ()
  (mref (all) 'harmonic-major))

(defun double-harmonic ()
  (mref (all) 'double-harmonic))

(defun hungarian-minor ()
  (mref (all) 'hungarian-minor))

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

(defun bebop-dominant ()
  (mref (all) 'bebop-dominant))

(defun bebop-dorian ()
  (mref (all) 'bebop-dorian))

(defun bebop-major ()
  (mref (all) 'bebop-major))

(defun bebop-melodic-minor ()
  (mref (all) 'bebop-melodic-minor))

(defun bebop-harmonic-minor ()
  (mref (all) 'bebop-harmonic-minor))

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

(defun c#-diminished ()
  (mref (all) 'c#-diminished))

(defun d-diminished ()
  (mref (all) 'd-diminished))

(defun eb-diminished ()
  (mref (all) 'eb-diminished))

;;; Persian

;;; Messiaen

;;; Other
