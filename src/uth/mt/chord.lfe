(defmodule uth.mt.chord
  (export (notes 3)))

;; Thin passthrough: the NIF already returns {ok, [#{pitch,octave}]} or
;; {error, <hyphenated-reason>}, so no LFE-side shaping is needed.
(defun notes (root quality number)
  (uth.mt.nif:chord-notes root quality number))
