(defmodule uth.mt.oth
  (export (base-space 0)
          (betweenness-centrality 1)))

;; Thin passthrough over the resource-handle NIFs. base-space/0 returns an
;; opaque handle; betweenness-centrality/1 runs the graph computation against it.
(defun base-space ()
  (uth.mt.nif:make-base-space))

(defun betweenness-centrality (space)
  (uth.mt.nif:betweenness-centrality space))
