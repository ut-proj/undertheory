(defmodule uth.interval-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest octaves-range-errors
  (let ((err-msg (uth.errors:octave-range)))
    (is-equal err-msg (uth.interval:name 1 2 -1))
    (is-equal err-msg (uth.interval:name 1 2 0))
    (is-equal err-msg (uth.interval:name 1 2 3))))

(deftest semitones-range-errors
  (let ((err-msg (uth.errors:semitone-range)))
    (is-equal err-msg (uth.interval:name -100 2 1))
    (is-equal err-msg (uth.interval:name -1 2 1))
    (is-equal err-msg (uth.interval:name 1 -1 1))
    (is-equal err-msg (uth.interval:name 1 25 1))
    (is-equal err-msg (uth.interval:name 1 200 1))))

(deftest name
  (is-equal 'M6 (uth.interval:name 'G# 'F 1))
  (is-equal 'm7 (uth.interval:name 'F# 'E 1))
  (is-equal 'M7 (uth.interval:name 'F 'E 1))
  (is-equal 'P8 (uth.interval:name 'F 'F 1)))

(deftest name-no-octave
  (is-equal 'M3 (uth.interval:name 'C 'E))
  (is-equal 'm3 (uth.interval:name 'C 'Eb))
  (is-equal 'P5 (uth.interval:name 'C 'G))
  (is-equal 'm6 (uth.interval:name 'C 'G#)))

(deftest name-one-octave
  (is-equal 'P0 (uth.interval:name 2 2 1))
  (is-equal 'P8 (uth.interval:name 2 14 1)))

(deftest name-two-octave
  (is-equal 'P0 (uth.interval:name 2 2 2))
  (is-equal 'P8 (uth.interval:name 2 14 2)))

(deftest norm
  (is-equal '(0 0) (uth.interval:norm 0 0))
  (is-equal '(0 12) (uth.interval:norm 0 12))
  (is-equal '(0 24) (uth.interval:norm 0 24))
  (is-equal '(12 24) (uth.interval:norm 12 0))
  (is-equal '(24 24) (uth.interval:norm 24 0)) ; don't like this ...
  (is-equal '(17 24) (uth.interval:norm 17 0)))

(deftest above
  (is-equal 'Eb (uth.interval:above 'C 'm3 #(flat)))
  (is-equal 'C# (uth.interval:above 'F# 'P5 #(sharp)))
  (is-equal 'B (uth.interval:above 'G 'M10 #(sharp)))
  )