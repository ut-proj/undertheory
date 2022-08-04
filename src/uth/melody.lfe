(defmodule uth.melody
  (export all))

(defun default-model ()
  #m(first-note-max-index 3
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

(defun base-note-count
  ((`#m(time-sig #(,beats ,note-value) bars ,bars))
   (* beats bars)))

(defun first-note (scale)
  (first-note scale (default-first-notes)))

(defun first-note (scale max)
  (lists:nth
   (rand:uniform max)
   (lists:sublist scale max)))

(defun last-notes (scale-name)
  (let ((scale (mref (uth.scale:all) 'scale-name)))
    (case (rand:uniform 2)
      (1 (list (lists:nth 2 scale) 1))
      (2 (list (lists:last scale) 8)))))

;; TODO:
;; - reduce over state ... current notes, min, max, count, % in range
