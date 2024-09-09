;;;; This module is a convenience wrapper for plural operations in the the
;;;; `uth.note` module.
(defmodule uth.notes
  (export all))

(defun ->intervals (notes) (uth.note:->intervals notes))
(defun ->intervals (previous notes acc) (uth.note:->intervals previous notes acc))

(defun transpose (root notes) (uth.note:transpose root notes))
(defun transpose (root notes opts) (uth.note:transpose root notes opts))

(defun invert (notes) (uth.note:invert notes))
(defun invert (root notes) (uth.note:invert root notes))
(defun invert (root notes opts) (uth.note:invert root notes opts))
