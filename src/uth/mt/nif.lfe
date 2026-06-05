(defmodule uth.mt.nif
  (on_load (init 0))
  (export (pong 0)))

(defun init ()
  (let ((path (filename:join
                (code:priv_dir 'undertheory)
                "crates/uth_mt_nif/uth_mt_nif")))
    (erlang:load_nif path 0)))

(defun pong ()
  (erlang:nif_error 'not-loaded))
