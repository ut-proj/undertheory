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

(deftest chord-notes-c-major-triad
  (is-equal #(ok (#m(pitch C octave 4)
                  #m(pitch E octave 4)
                  #m(pitch G octave 4)))
    (uth.mt.chord:notes 'C 'major 'triad)))

(deftest chord-notes-c-minor-triad
  (is-equal #(ok (#m(pitch C octave 4)
                  #m(pitch Eb octave 4)
                  #m(pitch G octave 4)))
    (uth.mt.chord:notes 'C 'minor 'triad)))

(deftest chord-notes-unknown-quality
  (is-equal #(error unknown-quality)
    (uth.mt.chord:notes 'C 'banana 'triad)))

(deftest base-space-cardinality
  (let ((space (uth.mt.oth:base-space)))
    (is (is_reference space))))

(deftest betweenness-centrality-cardinality
  (let* ((space (uth.mt.oth:base-space))
         (bc (uth.mt.oth:betweenness-centrality space)))
    (is-equal 228 (length bc))))
