(defmodule uth.mt.oth
  (export (base-space 0)
          (betweenness-centrality 1)))

;; base-space/0 caches one ResourceArc handle in persistent_term: the OTH base
;; space is constructed once per VM lifetime (BaseSpace::new is ~20 ms) and the
;; same opaque handle is returned on every later call (a sub-microsecond get).
;; No invalidation — the cached handle lives for the VM lifetime (S2 scope).
(defun base-space ()
  (case (persistent_term:get 'uth.mt.oth.base-space 'undefined)
    ('undefined
     (let ((handle (uth.mt.nif:make-base-space)))
       (persistent_term:put 'uth.mt.oth.base-space handle)
       handle))
    (handle handle)))

(defun betweenness-centrality (space)
  (uth.mt.nif:betweenness-centrality space))
