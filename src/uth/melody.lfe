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

(defun new-model (scale-template)
  (new-model (default-model)))

(defun new-model (scale-template model)
  (let* ((scale (uth.pitch:template-> scale-template))
         (`#m(first ,first-notes index ,starting) (first-notes scale model)))
    (clj:-> model
            (mupd 'scale scale)
            (mupd 'first first-notes)
            (mupd 'starting-index starting)
            (mupd 'first-count (length first-notes))
            (mupd 'base-count (base-note-count model)))))

(defun make ()
  "
     lfe> (uth.melody:make #m(bars 4)
     

  "
  )

(defun min (scale)
  (car scale))

(defun max ()
  (max (default-model)))

(defun max
  ((`#m(max-interval ,max))
   max))
  
(defun last-notes
  ((scale current) (when (> current 3))
   (list 8 (lists:last scale)))
  ((scale _)
   (list 1 (lists:nth 2 scale))))

(defun random-walk (scale-template)
  (random-walk scale-template (default-model)))

(defun random-walk (scale-template model)
  (random-walk (new-model scale-template model) 0 '()))

(defun random-walk
  (((= `#m(scale ,scale) model) previous-index '())
   (let ((`#m(first ,first index ,index) (first-notes scale model)))
     (random-walk model index first)))
  (((= `#m(scale ,s generate-count ,gc first-count ,fc) model) previous-index acc) (when (== (length acc) (+ gc fc)))
   (lists:reverse (lists:append (last-notes s (car acc)) acc)))
  (((= `#m(scale ,scale) model) previous-index acc)
   (let ((next-index (next previous-index (min scale) (max model))))
     (random-walk model
                  next-index
                  (lists:append
                   (list (lists:nth next-index scale))
                   acc)))))

(defun next (previous-index min max)
  (next previous-index 1 min max))

(defun next
  ((previous-index jump-interval min _) (when (== previous-index min))
   (+ min jump-interval))
  ((previous-index jump-interval _ max) (when (== previous-index max))
   (- max jump-interval))
  ((previous-index jump-interval _ _)
   (50-50
    (- previous-index jump-interval)
    (+ previous-index jump-interval))))

;; Supporting constructor functions

(defun base-note-count ()
  (base-note-count (default-model)))

(defun base-note-count
  ((`#m(time-sig #(,beats ,note-value) bars ,bars))
   (* beats bars)))

(defun generate-note-count (scale model)
  (- (base-note-count model)
     (length (first-notes scale model))
     2))

(defun first-notes (scale)
  (first-notes scale (default-model)))

(defun first-notes
  ((scale `#m(first-note-indices ,possibles))
   (first-notes scale possibles (rand:normal))))

(defun first-notes
  ((scale possibles select) (when (> select 0.667))
   `#m(first (list (lists:nth (lists:nth 3 possibles) scale))
       index 3))
  ((scale possibles select) (when (> select 0.333))
   `#m(first (list (lists:nth (lists:nth 2 possibles) scale))
       index 2))
  ((scale possibles select)
   `#m(first (list (lists:nth (lists:nth 1 possibles) scale))
       index 1)))

;; Utility functions

(defun 50-50 (choice-1 choice-2)
  (case (>= (rand:normal) 0.5)
    ('true choice-1)
    ('false choice-2)))

;; TODO:
;; - reduce over state ... current notes, min, max, count, % in range
