(defmodule uth.note
  (export all))

(defun whole () 1)
(defun half () 0.5)
(defun quarter () 0.25)
(defun eighth () 0.125)
(defun sixteenth () 0.0625)
(defun thirty-second () 0.03125)
(defun sixty-fourth () 0.015625)

;; Dotted notes

(defun dottend-half ()
  (+ (half) (quarter)))

(defun dottend-quarter ()
  (+ (quarter) (eighth)))

(defun dottend-eighth ()
  (+ (eighth) (sixteenth)))

(defun dottend-sixteenth ()
  (+ (sixteenth) (thirty-second)))

(defun dottend-thirty-second ()
  (+ (thirty-second) (sixty-fourth)))
