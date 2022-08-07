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
