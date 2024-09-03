(defmodule uth.scale-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest accidental
  (let ((result (list-comp ((<- x (uth.scale:names)))
                  (uth.scale:accidental x))))
    (is-equal '(natural flat natural sharp flat natural sharp flat natural sharp
                flat natural sharp flat natural sharp flat natural sharp natural
                flat natural sharp flat natural sharp flat natural sharp flat
                natural sharp flat natural sharp flat natural sharp flat natural)
              result)))

(deftest as-intervals
  (is-equal '(P0 M2 M3 a4 P5 M6 M7)
            (uth.scale:as-intervals (uth.scale:lydian)))
  (is-equal '(P0 M2 m3 P4 P5 m6 m7)
            (uth.scale:as-intervals (uth.scale:aeolian)))
  (is-equal '(P0 m2 m3 P4 dim5 m6 m7)
            (uth.scale:as-intervals (uth.scale:locrian))))

(deftest as-notes
  (is-equal '(C D E F# G A B)
            (uth.scale:as-notes 'C (uth.scale:lydian) #(sharp)))
  (is-equal '(C D Eb F G Ab Bb)
            (uth.scale:as-notes 'C (uth.scale:aeolian) #(flat)))
  (is-equal '(C Db Eb F Gb Ab Bb)
            (uth.scale:as-notes 'C (uth.scale:locrian) #(flat)))

  (is-equal '(Bb C D Eb F G A)
            (uth.scale:as-notes 'Bb (uth.scale:ionian)))
  (is-equal '(Bb C D E F G A)
            (uth.scale:as-notes 'Bb (uth.scale:lydian)))
  (is-equal '(F# G# A B C# D E)
            (uth.scale:as-notes 'F# (uth.scale:aeolian)))
  (is-equal '(Gb G A B C D E)
            (uth.scale:as-notes 'Gb (uth.scale:locrian))))