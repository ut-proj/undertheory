(defmodule uth-note-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest duration-fn-4-4
  (let ((notes->dur uth.note:duration-fn 120 #(4 4)))
    (is-equal 1
              (notes->dur 'whole))
    (is-equal 1
              (notes->dur 'w))
    (is-equal 1
              (notes->dur 'n/2))
    (is-equal 1
              (notes->dur 'n/4))
    (is-equal 1
              (notes->dur 'n/8))
    (is-equal 1
              (notes->dur 'n/16))
    (is-equal 1
              (notes->dur 'n/32))
    (is-equal 1
              (notes->dur 'n/64))))

(deftest duration-fn-list-4-4
  (let ((notes->dur uth.note:duration-fn 120 #(4 4)))
    (is-equal 1
              (notes->dur '(n/32 n/32)))
    (is-equal 1
              (notes->dur '(n/2 n/8 n/32)))
    (is-equal 1
              (notes->dur '(n/4 n/4 n/4 n/4)))))

(deftest duration-fn-dotteds-4-4
  (let ((notes->dur uth.note:duration-fn 120 #(4 4)))
    (is-equal 1
              (notes->dur '(n/2. n/4)))
    (is-equal 1
              (notes->dur '(n/4. n/4. n/4)))
    (is-equal 1
              (notes->dur '(n/8. n/8. n/4. n/4)))
    (is-equal 1
              (notes->dur '(n/16. n/16. n/8. n/4. n/4)))
    (is-equal 1
              (notes->dur '(n/32. n/32. n/16. n/8. n/4. n/4)))))