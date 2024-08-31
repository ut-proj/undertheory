(defmodule uth.errors
  (export all))

(defun note-name ()
  #(error "Supplied name must be a legal note name atom; see (uth.note:names) for allowed values."))
(defun octave-range ()
  #(error "octave number must be either 1 or 2"))

(defun semitone-range ()
  #(error "numeric note values must be integers between 0 and 24, inclusive"))
