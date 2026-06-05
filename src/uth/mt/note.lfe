(defmodule uth.mt.note
  (export (parse-midi-pitch 1)))

;; Thin LFE-idiomatic face over the NIF. Two pieces of shaping:
;;   - the NIF decodes a binary (Rustler), so convert the LFE charlist input;
;;   - present the NIF's snake_case error atom in LFE-idiomatic hyphenated form.
(defun parse-midi-pitch (s)
  (case (uth.mt.nif:parse-midi-pitch (erlang:iolist_to_binary s))
    (`#(error invalid_pitch) #(error invalid-pitch))
    (result result)))
