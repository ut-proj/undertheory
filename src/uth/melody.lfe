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
;;;; Doing so makes the maths _a lot_ easier.
;;;;
;;;; A such, all scale inputs for the functions in this module should be of the
;;;; latter form, ready to be mathed.

(defmodule uth.melody
  (export all))

(defun default-model ()
  #m(first-note-indices (1 3 5)
     majority-range 6
     majority-percent 0.6
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

(defun make
  "
     lfe> (uth.melody:make #m(bars 4)
     

  "
  )

(defun base-note-count ()
  (base-note-count (default-model)))

(defun base-note-count
  ((`#m(time-sig #(,beats ,note-value) bars ,bars))
   (* beats bars)))

(defun first-notes (scale)
  (first-notes scale (default-model)))

(defun first-notes
  ((scale `#m(first-note-indices ,possibles))
   (first-notes scale possibles (rand:normal))))

(defun first-notes
  ((scale possibles select) (when (> select 0.667))
   (lists:nth (lists:nth 3 possibles) scale))
  ((scale possibles select) (when (> select 0.333))
   (lists:nth (lists:nth 2 possibles) scale))
  ((scale possibles select)
   (lists:nth (lists:nth 1 possibles) scale)))
  
(defun last-notes (scale)
  (50-50
   (list 1 (lists:nth 2 scale))
   (list 8 (lists:last scale))))

(defun random-walk (scale)
  (random-walk scale (default-model)))

(defun random-walk
  ((scale (= `#m(max-interval ,max) model))
  (random-walk scale
               (- (base-note-count model)
                  (length (first-notes scale model))
                  (length (last-notes scale)))
               (car scale)
               (lists:nth max scale)))) ; TODO: this last one is incorrect; we'll fix it when we move away from templates

(defun random-walk (scale max-count min max)
  (random-walk (first-note scale)
               (last-notes scale)
               max-count
               min
               max))

(defun random-walk
  ((scale (= `#m() model) previous-index last-notes max-count min max) (when (is_integer max))
   (random-walk max-count min max `(,start)))
  ((scale (= `#m() model) max-count last-notes min max acc) (when (andalso (is_list acc) (== (length acc) (- max-count (+ 1 (length last-notes))))))
   (lists:reverse (lists:append last-notes acc)))
  ((scale (= `#m() model) max-count last-notes min max acc) (when (is_list acc))
   (random-walk max-count
                last-notes
                min
                max
                (lists:append
                 (next (car acc) min max)
                 acc))))
  
(defun next
  ((current min _) (when (== current min))
   (list (+ min 1)))
  ((current _ max) (when (== current max))
   (list (- max 1)))
  ((current min max)
   (list (50-50
          (- current 1)
          (+ current 1)))))

(defun 50-50 (choice-1 choice-2)
  (case (>= (rand:normal) 0.5)
    ('true choice-1)
    ('false choice-2)))

;; TODO:
;; - reduce over state ... current notes, min, max, count, % in range
