;;;; This package is focused on the algorithmic generation of melody.
;;;;
;;;; Eventually, multiple melody-generation models will live here, but starting
;;;; simply for now.
;;;;
;;;; Note that, while the scale module uses "scale templates" of this form,
;;;; for example:
;;;;
;;;;    1  2  b3 4  5  b6 b7
;;;;    1  2  3  4  5  6  7
;;;;
;;;; this melody module uses numbered intervals instead, like this:
;;;;
;;;;    0  2  3  5  7  8  10
;;;;    0  2  4  5  7  9  11
;;;;
;;;; Doing so makes the maths _a lot_ easier. The only exceptions to this are
;;;; the various constructor functions.
;;;;
;;;; A such, most scale inputs for the functions in this module should be of the
;;;; latter form, ready to be mathed.

(defmodule uth.melody
  (export all))

(defun default-model ()
  `#m(generator-fn ,#'uth.melody:random-walk/1
     default-scale 'aeolian
     first-note-indices (1 3 5)
     final-note-count 2
     reverse-chance 0.5
     majority-range 6
     majority-percent 0.6
     min-interval 1
     max-interval 10
     time-signature #(4 4)
     bars 2
     chance-for-2nd 0.5
     chance-for-3rd 0.2
     chance-for-4th 0.1
     chance-for-5th 0.1
     chance-for-6th 0.025
     chance-for-7th 0.025
     chance-for-8th 0.05
     chance-for-direction-change 0.25))

(defun new-model (scale-name)
  (new-model scale-name (default-model)))

(defun new-model
  ((scale-name (= `#m(max-interval ,i) model))
   (let* ((scale (uth.sequence:make scale-name `#m(type max interval ,i)))
          (`#m(first ,first-notes index ,starting) (first-notes scale model)))
     (clj:-> model
             (mset 'base-scale-length (length (mref (uth.scale:all) scale-name)))
             (mset 'scale scale)
             (mset 'first first-notes)
             (mset 'starting-index starting)
             (mset 'first-count (length first-notes))
             (mset 'base-count (base-note-count model))
             (mset 'generate-count (generate-note-count scale model))))))

(defun make (scale-name)
  "
     lfe> (uth.melody:make #m(bars 4)


  "
  (make scale-name #m()))

(defun make (scale-name overrides)
  (make scale-name overrides (default-model)))

(defun make (scale-name overrides model)
  (new-model scale-name (maps:merge model overrides)))

(defun run
 (((= `#m(generator-fn ,generator-fn) model))
  (funcall generator-fn model)))

(defun min ()
  (min (default-model)))

(defun min
  ((`#m(min-interval ,min))
   (uth.pitch:template-note-> min)))

(defun max ()
  (max (default-model)))

(defun max
  ((`#m(max-interval ,max))
   (uth.pitch:template-note-> max)))

;; TODO: address larger intervals
(defun last-notes
  "This returns the final n notes of a scale.

  Note that the results are given in reverse order, since melodies are assembled
  by pre-pending notes (and reversed in the final step).
  "
  ((`#m(scale ()) _)
   '())
  ((`#m(scale ,s base-scale-length ,bsl) current) (when (> current 12))
   (list (+ 12 (car s)) (+ 12 (lists:nth 2 s))))
  ((`#m(scale ,s base-scale-length ,bsl) current) (when (> current 3))
   (list (+ 12 (car s)) (lists:nth bsl s)))
  ((`#m(scale ,s final-note-count ,fnc) _)
   (list (lists:nth 2 s) (car s))))


(defun random-walk
  ((`#m(scale ,s min-interval ,min max-interval ,max base-count ,bc))
   (let ((scale (extend-scale s 3)))
     (list-comp ((<- x (lists:reverse (random-walk min max bc '(0)))))
       (- (lists:nth (+ x 12) scale) 12)))))

(defun random-walk
 ((min max max-count acc) (when (>= (length acc) max-count))
   acc)
 ((min max max-count (= `(,prev . ,_) acc))
   (random-walk min
                max
                max-count
                (lists:append (list (next min max prev)) acc))))

(defun next
 ((min _ prev) (when (=< prev min))
  (+ prev 1))
 ((_ max prev) (when (>= prev max))
  (- prev 1))
 ((_ _ prev)
  (+ prev (direction))))

(defun invert
  (((= `#m(scale ,s base-scale-length ,bsl) model) melody)
   (let* ((melody-indices (index-melody model melody))
          (rel-inv-offsets (inverse-offsets (offsets melody-indices)))
          (inv-offsets (list-comp ((<- x rel-inv-offsets)) (+ (car melody-indices) x)))
          (t-val (get-transpose-val (lists:min inv-offsets)))
          (trans-offsets (transpose inv-offsets t-val))
          (scale-x (extend-scale s (get-scale-val (lists:max trans-offsets))))
          (unadj-inverted-melody (list-comp ((<- x inv-offsets)) (lists:nth (+ x bsl) scale-x))))
     (inverted-last-notes
      model
      (transpose unadj-inverted-melody (* -1 t-val))))))

;; Supporting constructor functions

(defun base-note-count ()
  (base-note-count (default-model)))

(defun base-note-count
  ((`#m(time-signature #(,beats ,note-value) bars ,bars))
   (* beats bars)))

(defun generate-note-count (scale)
  (generate-note-count scale (default-model)))

(defun generate-note-count (scale model)
  (- (base-note-count model)
     (length (mref (first-notes scale model) 'first))
     2))

(defun first-notes (scale)
  (first-notes scale (default-model)))

(defun first-notes
  ((scale `#m(first-note-indices ,possibles))
   (let* ((len (length possibles))
          (index (rand:uniform len)))
     `#m(first ,(list (lists:nth index possibles))
         index ,index))))

;; Utility functions

(defun direction ()
  (case (rand:uniform 2)
    (1 1)
    (2 -1)))

(defun offsets
  "This generates a list of offsets, relative to the first note of a melody."
  (((= `(,first . ,_) melody))
   (list-comp
     ((<- x melody))
     (- x first))))

(defun inverse-offsets (melody)
  (list-comp
    ((<- x (offsets melody)))
    (* x -1)))

(defun transpose (melody offset)
  (list-comp
    ((<- x melody))
    (+ offset x)))

(defun extend-scale (scale times)
  "This function takes a scale and extends it by the given amount.

  Note that this creates duplicate entries (due to overlap), so these are
  removed by uniquing this result."
  (lists:sort
    (lists:uniq
      (list-comp
        ((<- x scale)
         (<- y (lists:seq 0 (- times 1))))
        (+ (* y 12) x)))))

(defun get-transpose-val
  ((min) (when (>= min 0))
   0)
  ((min)
   (* 12 (+ 1 (div (abs min) 12)))))

(defun get-scale-val (max)
  (+ 1 (div max 12)))

(defun index-scale
  ((`#m(scale ,s) melody)
   (maps:from_list
    (lists:foldl
     (match-lambda
       ((x '())
        `(#(,x 1)))
       ((x (= `(#(,_ ,last) . ,_) acc))
        (lists:append `(#(,x ,(+ 1 last))) acc)))
     '()
     (extend-scale s (get-scale-val (lists:max melody)))))))

(defun index-melody (model melody)
  (let ((lookup (index-scale model melody)))
    (list-comp ((<- p melody)) (mref lookup p))))

(defun inverted-last-notes
  "This adjusts an inverted melody so that the last note falls on the tonic."
  ((`#m(scale ,s base-scale-length ,bsl) melody)
   (let ((head (lists:sublist melody (- (length melody) 2))))
     (case (lists:last head)
       (x (when (> x 12))
        (lists:append head (list (+ 12 (lists:nth 2 s)) (+ 12 (car s)))))
       (x (when (> x 3))
        (lists:append head (list (lists:nth bsl s) (+ 12 (car s)))))
       (x
        (lists:append head (list (lists:nth 2 s) (car s))))))))

