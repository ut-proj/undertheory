(defmodule uth.mt.nif-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest parse-midi-pitch-middle-c
  (is-equal #(ok 60)
    (uth.mt.note:parse-midi-pitch "C4")))

(deftest parse-midi-pitch-a440
  (is-equal #(ok 69)
    (uth.mt.note:parse-midi-pitch "A4")))

(deftest parse-midi-pitch-flat
  (is-equal #(ok 58)
    (uth.mt.note:parse-midi-pitch "Bb3")))

(deftest parse-midi-pitch-empty
  (is-equal #(error invalid-pitch)
    (uth.mt.note:parse-midi-pitch "")))
