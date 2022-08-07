(defmodule uth.melody-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest min
  (is-equal 0 (uth.melody:min)))

(deftest max
  (is-equal 16 (uth.melody:max)))

(deftest base-note-count
  (is-equal 8 (uth.melody:base-note-count)))

(deftest first-notes
  (is-equal
   'true
   (is_map (uth.melody:first-notes 'aeolian))))

(deftest generate-note-count
  (is-equal
   'true
   (is_integer (uth.melody:generate-note-count 'aeolian))))

(deftest last-notes
  (let* (((= `#m(scale ,s) m) (uth.melody:new-model 'aeolian)))
    (is-equal '(0 2 3 5 7 8 10 12 14 15) s)
    (is-equal 0 (lists:nth 1 s))
    (is-equal 15 (lists:nth 10 s))
    (is-equal '(0 2) (uth.melody:last-notes m (lists:nth 1 s)))
    (is-equal '(0 2) (uth.melody:last-notes m (lists:nth 2 s)))
    (is-equal '(0 2) (uth.melody:last-notes m (lists:nth 3 s)))
    (is-equal '(12 10) (uth.melody:last-notes m (lists:nth 4 s)))
    (is-equal '(12 10) (uth.melody:last-notes m (lists:nth 5 s)))
    (is-equal '(12 10) (uth.melody:last-notes m (lists:nth 6 s)))
    (is-equal '(12 10) (uth.melody:last-notes m (lists:nth 7 s)))
    (is-equal '(12 10) (uth.melody:last-notes m (lists:nth 8 s)))
    (is-equal '(12 14) (uth.melody:last-notes m (lists:nth 9 s)))
    (is-equal '(12 14) (uth.melody:last-notes m (lists:nth 10 s)))))
