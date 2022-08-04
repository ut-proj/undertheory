(defmodule uth.sequence
  (export all))

(defun octave-tones () 8)
(defun octave-semitones () 12)

(defun make
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
