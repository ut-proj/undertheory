(defmodule uth.pitch-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest template-note->
  (is-equal 0 (uth.pitch:template-note-> 1))
  (is-equal 4 (uth.pitch:template-note-> 3))
  (is-equal 7 (uth.pitch:template-note-> 5))
  (is-equal 10 (uth.pitch:template-note-> 'b7))
  (is-equal 12 (uth.pitch:template-note-> 8))
  (is-equal 14 (uth.pitch:template-note-> 9))
  (is-equal 18 (uth.pitch:template-note-> 11))
  (is-equal 22 (uth.pitch:template-note-> 13))
  (is-equal 9 (uth.pitch:template-note-> 14))
  (is-equal 11 (uth.pitch:template-note-> 15))
  (is-equal 12 (uth.pitch:template-note-> 16)))