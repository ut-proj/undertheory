(defmodule uth.mt.note
  (export (parse-midi-pitch 1)))

;; Thin passthrough: the NIF already returns {ok, N} or {error, invalid-pitch}
;; directly. The only shaping is coercing the LFE charlist input to the binary
;; Rustler's String decoder expects.
(defun parse-midi-pitch (s)
  (uth.mt.nif:parse-midi-pitch (erlang:iolist_to_binary s)))
