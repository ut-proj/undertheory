(defmodule uth.sequence
  (export all))

(defun octave-tones () 8)
(defun octave-semitones () 12)

(defun make
  "To generate a sequence that triples the notes in a given scale:

     lfe> (uth.sequence:make 'blues-minor-pentatonic #m(type mult times 3))
     (1 b3 4 b6 b7 1 b3 4 b6 b7 1 b3 4 b6 b7)

  To generate a sequence that extends the notes in a scale by 4:

     lfe> (uth.sequence:make 'locrian #m(type extend by 4))
     (1 b2 b3 4 b5 b6 b7 1 b2 b3 4)

  To generate a sequence that repeats a scale until a particular interval:

     lfe> (uth.sequence:make 'suspended-egyptian-pentatonic #m(type max interval 13))
     (1 2 4 5 b7 1)
  "
  ((scale-name opts) (when (is_list scale-name))
   (make (list_to_atom scale-name) opts))
  ((scale-name `#m(type mult times ,times))
   (mult scale-name times))
  ((scale-name `#m(type extend by ,amount))
   (extend scale-name amount))
  ((scale-name `#m(type max interval ,number))
   (intervening scale-name number)))

(defun mult
  ((scale-name times) (when (is_atom scale-name))
   (mult (mref (uth.scale:all) scale-name) times))
  ((scale times)
   (list-comp ((<- _ (lists:seq 0 (- times 1)))
               (<- note scale))
     note)))

(defun extend (scale-name amount)
  (let* ((scale (mref (uth.scale:all) scale-name))
         (len (length scale))
         (total (+ len amount))
         (times (ceil (/ total len))))
    (lists:sublist (mult scale times) total)))

(defun intervening
  ((scale-name number) (when (is_atom scale-name))
   (intervening (mref (uth.scale:all) scale-name) number))
  ((scale number)
   (let ((max (ceil (/ (+ number 1) (octave-tones)))))
     (lists:filtermap
      (lambda (x) x)
      (list-comp ((<- l (lists:seq 0 (- max 1)))
                  (<- note scale))
        (case (+ (interval note) (* l (octave-semitones)))
          (ival (when (=< ival number))
                `#(true ,note))
          (_ 'false)))))))

(defun interval
  ((scale-element) (when (is_integer scale-element))
   scale-element)
  ((scale-element) (when (is_atom scale-element))
   (list_to_integer (list (lists:last (atom_to_list scale-element))))))
