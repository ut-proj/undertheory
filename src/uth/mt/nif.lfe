(defmodule uth.mt.nif
  (on_load (init 0))
  (export (pong 0)
          (parse-midi-pitch 1)
          (chord-notes 3)))

(defun init ()
  (let ((path (filename:join
                (code:priv_dir 'undertheory)
                "crates/uth_mt_nif/uth_mt_nif")))
    (erlang:load_nif path 0)))

(defun pong ()
  (erlang:nif_error 'not-loaded))

(defun parse-midi-pitch (_)
  (erlang:nif_error 'not-loaded))

(defun chord-notes (_ _ _)
  (erlang:nif_error 'not-loaded))
