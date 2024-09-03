;;;; This module is **not** MIDI-aware; as such, all inputs need to be converted
;;;; to values in the twleve tone scale (either one or two octaves: 0-12 or
;;;; 0-24).
(defmodule uth.interval
  (export all))

(defun names ()
  '(P0 m2 M2 m3 M3 P4 a4 dim5 P5 m6 M6 m7 M7
    P8 m9 M9 m10 M10 P11 a11 dim12 P12 m13 M13 m14 M14 ; not 100% about all of these; need to 
    P15))                                              ; check multiple sources ...

(defun numbers ()
  '(0 1 2 3 4 5 6 6 7 8 9 10 11
    12 13 14 15 16 17 18 18 19 20 21 22 23
    24))

(defun name->number () (lists:zip (names) (numbers)))
(defun number->name () (lists:zip (numbers) (names)))

(defun lookup-names (number) (proplists:get_all_values number (number->name)))
(defun lookup-number (name) (proplists:get_value name (name->number)))

(defun norm (note-num1 note-num2)
  (cond
   ((=< note-num1 note-num2) (list note-num1 note-num2))
   ('true
    (if (== 0 (uth.note:mod-1oct (abs (- note-num1 note-num2))))
      (norm note-num1 (+ note-num2 (uth.note:2oct)))
      (norm note-num1 (+ note-num2 (uth.note:1oct)))))))

(defun name (note1 note2)
  (name note1 note2 1))

(defun name
  ((_ _ os) (when (orelse (< os 1) (> os 2)))
   (uth.errors:octave-range))
  ((note1 note2 octaves) (when (andalso (is_atom note1) (is_atom note2)))
   (let ((n1 (uth.note:number note1 'with-error))
         (n2 (uth.note:number note2 'with-error)))
     (case (list n1 n2)
       (`(#(error ,_) ,_) n1)
       (`(,_ #(error ,_)) n2)
       (`(,_ ,_)
        (let ((diff (- n2 n1)))
          (if (== diff 0)
            (name n1 (+ n2 12) octaves)
            (name n1 n2 octaves)))))))
  ((n1 n2 _) (when (orelse (< n1 0) (> n1 24) (< n2 0) (> n2 24)))
   (uth.errors:semitone-range))
  ((num1 num2 octaves)
   (let* ((`(,n1 ,n2) (norm num1 num2))
          (raw (- n2 n1))
          (intv (cond
                 ((and (== octaves 2) (== raw (uth.note:2oct))) (uth.note:2oct))
                 ((== octaves 2) (rem raw (uth.note:2oct)))
                 ((== raw (uth.note:1oct)) (uth.note:1oct))
                 ('true (rem raw (uth.note:1oct))))))
     (car (lookup-names intv)))))

(defun above (low-note-name intv-name)
  (above low-note-name intv-name #(all)))

(defun above (low-note-name intv-name opts)
  (let ((note (uth.note:number low-note-name))
        (intv (lookup-number intv-name)))
    (uth.note:name (uth.note:mod-1oct (+ note intv)) opts)))

(defun below (high-note-name intv-name)
  )