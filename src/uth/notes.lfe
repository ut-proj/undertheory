;;;; This module is a convenience wrapper for plural operations in the the
;;;; `uth.note` module.
(defmodule uth.notes
  (export all))

(defun ->intervals (notes) (uth.note:->intervals notes))
(defun ->intervals (previous notes acc) (uth.note:->intervals previous notes acc))

(defun invert (root notes) (uth.note:invert root notes))
(defun invert (root notes opts) (uth.note:invert root notes opts))
