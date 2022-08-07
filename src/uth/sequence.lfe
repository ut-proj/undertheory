(defmodule uth.sequence
  (export all))

(defun octave-tones () 8)
(defun octave-semitones () 12)

(defun default-format ()
  #m(format scale))

(defun default-opts ()
  (maps:merge
   (default-format)
   #m(type mult
      times 1)))

(defun make (scale-name)
  (make scale-name (default-opts)))

(defun make
  "To generate a sequence that triples the notes in a given scale:

     lfe> (uth.sequence:make 'blues-minor-pentatonic #m(type mult times 3))
     (0 3 5 8 10 12 15 17 20 22 24 27 29 32 34)
     lfe> (uth.sequence:make 'blues-minor-pentatonic #m(type mult times 3 format scale-template))
     (1 b3 4 b6 b7 1 b3 4 b6 b7 1 b3 4 b6 b7)

  To generate a sequence that extends the notes in a scale by 4:

     lfe> (uth.sequence:make 'locrian #m(type extend by 4))
     (0 1 3 5 6 8 10 12 13 15 17)
     lfe> (uth.sequence:make 'locrian #m(type extend by 4 format scale-template))
     (1 b2 b3 4 b5 b6 b7 1 b2 b3 4)

  To generate a sequence that repeats a scale until a particular interval:

     lfe> (uth.sequence:make 'suspended-egyptian-pentatonic #m(type max interval 11))
     (0 2 5 7 10 12 14 17)
     lfe> (uth.sequence:make 'suspended-egyptian-pentatonic #m(type max interval 11 format scale-template))
     (1 2 4 5 b7 1 2 4)
  "
  ((scale-name opts) (when (is_list scale-name))
   (make (list_to_atom scale-name) (maps:merge (default-opts) opts)))
  ((scale-name (= `#m(type mult times ,times) opts))
   (mult scale-name times (maps:merge (default-opts) opts)))
  ((scale-name (= `#m(type extend by ,amount) opts))
   (extend scale-name amount (maps:merge (default-opts) opts)))
  ((scale-name (= `#m(type max interval ,number) opts))
   (intervening scale-name number (maps:merge (default-opts) opts))))

(defun mult
  ((scale-name times opts) (when (is_atom scale-name))
   (mult (mref (uth.scale:all) scale-name) times opts))
  ((scale times `#m(format ,format))
   (list-comp ((<- x (lists:seq 0 (- times 1)))
               (<- note scale))
     (case format
       ('scale-template note)
       (_ (+ (uth.pitch:template-note-> note) (* 12 x)))))))

(defun extend
  ((scale-name amount opts)
   (let* ((scale (mref (uth.scale:all) scale-name))
          (len (length scale))
          (total (+ len amount))
          (times (ceil (/ total len))))
     (lists:sublist (mult scale times opts) total))))

(defun intervening
  ((scale-name number opts) (when (is_atom scale-name))
   (intervening (mref (uth.scale:all) scale-name) number opts))
  ((scale number `#m(format ,format))
   (let ((max (ceil (/ (+ number 1) (octave-tones)))))
     (lists:filtermap
      (lambda (x) x)
      (list-comp ((<- x (lists:seq 0 (- max 1)))
                  (<- note scale))
        (let ((scale-note (+ (uth.pitch:template-note-> note) (* x (octave-semitones))))
              (scale-number (uth.pitch:template-note-> number)))
          (case scale-note
            (_ (when (=< scale-note scale-number))
                  (case format
                    ('scale-template `#(true ,note))
                    (_ `#(true ,scale-note))))
            (_ 'false))))))))

(defun interval
  ((scale-element) (when (is_integer scale-element))
   scale-element)
  ((scale-element) (when (is_atom scale-element))
   (list_to_integer (list (lists:last (atom_to_list scale-element))))))
