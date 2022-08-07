(defmodule uth.sequence-tests
  (behaviour ltest-unit)
  (export all))

(include-lib "ltest/include/ltest-macros.lfe")

(deftest make-default
  (is-equal
   '(0 3 5 8 10)
   (uth.sequence:make 'blues-minor-pentatonic)))

(deftest mult-scale
  (is-equal
   '(0 3 5 8 10 12 15 17 20 22 24 27 29 32 34)
   (uth.sequence:make 'blues-minor-pentatonic #m(type mult times 3))))

(deftest mult-scale-template
  (is-equal
   '(1 b3 4 b6 b7 1 b3 4 b6 b7 1 b3 4 b6 b7)
   (uth.sequence:make 'blues-minor-pentatonic #m(type mult times 3 format scale-template))))

(deftest extend-scale
  (is-equal
   '(0 1 3 5 6 8 10 12 13 15 17)
   (uth.sequence:make 'locrian #m(type extend by 4))))

(deftest extend-scale-template
  (is-equal
   '(1 b2 b3 4 b5 b6 b7 1 b2 b3 4)
   (uth.sequence:make 'locrian #m(type extend by 4 format scale-template))))

(deftest interval-scale
  (is-equal
   '(0 2 4 5 7 9 11 12 14 16)
   (uth.sequence:make 'ionian #m(type max interval 10)))
  (is-equal
   '(0 2 4 5 7 9 11 12 14 16 17 19 21)
   (uth.sequence:make 'ionian #m(type max interval 13)))
  (is-equal
   '(0 3 5 8 10 12 15 17 20 22)
   (uth.sequence:make 'blues-minor-pentatonic #m(type max interval 13)))
  (is-equal
   '(0 1 3 5 6 8 10 12 13 15)
   (uth.sequence:make 'locrian #m(type max interval 10)))
  (is-equal
   '(0 1 3 5 6 8 10 12 13 15 17 18 20 22)
   (uth.sequence:make 'locrian #m(type max interval 13)))
  (is-equal
   '(0 2 5 7 10 12 14 17 19 22)
   (uth.sequence:make 'suspended-egyptian-pentatonic #m(type max interval 13))))

(deftest interval-scale-template
  (is-equal
   '(1 2 3 4 5 6 7 1 2 3)
   (uth.sequence:make 'ionian #m(type max interval 10 format scale-template)))
  (is-equal
   '(1 2 3 4 5 6 7 1 2 3 4 5 6)
   (uth.sequence:make 'ionian #m(type max interval 13 format scale-template)))
  (is-equal
   '(1 b3 4 b6 b7 1 b3 4 b6 b7)
   (uth.sequence:make 'blues-minor-pentatonic #m(type max interval 13 format scale-template)))
  (is-equal
   '(1 b2 b3 4 b5 b6 b7 1 b2 b3)
   (uth.sequence:make 'locrian #m(type max interval 10 format scale-template)))
  (is-equal
   '(1 b2 b3 4 b5 b6 b7 1 b2 b3 4 b5 b6 b7)
   (uth.sequence:make 'locrian #m(type max interval 13 format scale-template)))
  (is-equal
   '(1 2 4 5 b7 1 2 4 5 b7)
   (uth.sequence:make 'suspended-egyptian-pentatonic #m(type max interval 13 format scale-template))))