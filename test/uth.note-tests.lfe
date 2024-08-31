(defmodule uth.note-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest duration-fn-4-4
  (let ((notes->dur (uth.note:duration-fn 120 #(4 4))))
    (is-equal 2000
              (funcall notes->dur '1))
    (is-equal 1000
              (funcall notes->dur '1/2))
    (is-equal 500
              (funcall notes->dur '1/4))
    (is-equal 250
              (funcall notes->dur '1/8))
    (is-equal 125
              (funcall notes->dur '1/16))
    (is-equal 62
              (funcall notes->dur '1/32))
    (is-equal 31
              (funcall notes->dur '1/64))))

;;(deftest duration-fn-list-4-4
;;  (let ((notes->dur (uth.note:duration-fn 120 #(4 4))))
;;    (is-equal 1
;;              (funcall notes->dur '(1/32 1/32)))
;;    (is-equal 1
;;              (funcall notes->dur '(1/2 1/8 1/32)))
;;    (is-equal 1
;;              (funcall notes->dur '(1/4 1/4 1/4 1/4)))))

;;(deftest duration-fn-dotteds-4-4
;;  (let ((notes->dur (uth.note:duration-fn 120 #(4 4))))
;;    (is-equal 1
;;              (funcall notes->dur '(1/2. 1/4)))
;;    (is-equal 1
;;              (funcall notes->dur '(1/4. 1/4. 1/4)))
;;    (is-equal 1
;;              (funcall notes->dur '(1/8. 1/8. 1/4. 1/4)))
;;    (is-equal 1
;;              (funcall notes->dur '(1/16. 1/16. 1/8. 1/4. 1/4)))
;;    (is-equal 1
;;              (funcall notes->dur '(1/32. 1/32. 1/16. 1/8. 1/4. 1/4)))))

(deftest number-enharmonics
  (is-equal (uth.note:number 'C) (uth.note:number 'B#))
  (is-equal (uth.note:number 'E) (uth.note:number 'Fb))
  (is-equal (uth.note:number 'F) (uth.note:number 'E#))
  (is-equal (uth.note:number 'B) (uth.note:number 'Cb)))

(deftest number-double-flats
  (is-equal (uth.note:number 'Bb) (uth.note:number 'Cbb))
  (is-equal (uth.note:number 'C) (uth.note:number 'Dbb))
  (is-equal (uth.note:number 'D) (uth.note:number 'Ebb))
  (is-equal (uth.note:number 'Eb) (uth.note:number 'Fbb))
  (is-equal (uth.note:number 'F) (uth.note:number 'Gbb))
  (is-equal (uth.note:number 'G) (uth.note:number 'Abb))
  (is-equal (uth.note:number 'A) (uth.note:number 'Bbb)))

(deftest number-double-sharps
  (is-equal (uth.note:number 'D) (uth.note:number 'C##))
  (is-equal (uth.note:number 'E) (uth.note:number 'D##))
  (is-equal (uth.note:number 'G) (uth.note:number 'F##))
  (is-equal (uth.note:number 'A) (uth.note:number 'G##))
  (is-equal (uth.note:number 'B) (uth.note:number 'A##))
  (is-equal (uth.note:number 'C#) (uth.note:number 'B##)))

(deftest name
  (is-equal 'C (uth.note:name 0))
  (is-equal 'C# (uth.note:name 1))
  (is-equal 'D (uth.note:name 2))
  (is-equal 'D# (uth.note:name 3))
  (is-equal 'E (uth.note:name 4))
  (is-equal 'F (uth.note:name 5))
  (is-equal 'F# (uth.note:name 6))
  (is-equal 'G (uth.note:name 7))
  (is-equal 'G# (uth.note:name 8))
  (is-equal 'A (uth.note:name 9))
  (is-equal 'A# (uth.note:name 10))
  (is-equal 'B (uth.note:name 11)))

(deftest name-all
  (is-equal '(C B# Dbb) (uth.note:name 0 #(all)))
  (is-equal '(C# Db B##) (uth.note:name 1 #(all)))
  (is-equal '(D Ebb C##) (uth.note:name 2 #(all)))
  (is-equal '(D# Eb Fbb) (uth.note:name 3 #(all)))
  (is-equal '(E Fb D##) (uth.note:name 4 #(all)))
  (is-equal '(F E# Gbb) (uth.note:name 5 #(all)))
  (is-equal '(F# Gb E##) (uth.note:name 6 #(all)))
  (is-equal '(G Abb F##) (uth.note:name 7 #(all)))
  (is-equal '(G# Ab) (uth.note:name 8 #(all)))
  (is-equal '(A Bbb G##) (uth.note:name 9 #(all)))
  (is-equal '(A# Bb Cbb) (uth.note:name 10 #(all)))
  (is-equal '(B Cb A##) (uth.note:name 11 #(all))))

(deftest name-flats
  (is-equal 'C (uth.note:name 0 #(flat)))
  (is-equal 'Db (uth.note:name 1 #(flat)))
  (is-equal 'D (uth.note:name 2 #(flat)))
  (is-equal 'Eb (uth.note:name 3 #(flat)))
  (is-equal 'E (uth.note:name 4 #(flat)))
  (is-equal 'F (uth.note:name 5 #(flat)))
  (is-equal 'Gb (uth.note:name 6 #(flat)))
  (is-equal 'G (uth.note:name 7 #(flat)))
  (is-equal 'Ab (uth.note:name 8 #(flat)))
  (is-equal 'A (uth.note:name 9 #(flat)))
  (is-equal 'Bb (uth.note:name 10 #(flat)))
  (is-equal 'B (uth.note:name 11 #(flat))))

(deftest name-sharps
  (is-equal 'C (uth.note:name 0 #(sharp)))
  (is-equal 'C# (uth.note:name 1 #(sharp)))
  (is-equal 'D (uth.note:name 2 #(sharp)))
  (is-equal 'D# (uth.note:name 3 #(sharp)))
  (is-equal 'E (uth.note:name 4 #(sharp)))
  (is-equal 'F (uth.note:name 5 #(sharp)))
  (is-equal 'F# (uth.note:name 6 #(sharp)))
  (is-equal 'G (uth.note:name 7 #(sharp)))
  (is-equal 'G# (uth.note:name 8 #(sharp)))
  (is-equal 'A (uth.note:name 9 #(sharp)))
  (is-equal 'A# (uth.note:name 10 #(sharp)))
  (is-equal 'B (uth.note:name 11 #(sharp))))
