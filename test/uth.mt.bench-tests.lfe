;; Boundary-overhead benchmark suite (Arc 2 S5). Not an ltest suite — a plain
;; module of bench functions run from the REPL. Each returns a map with the
;; per-call median/p95 (distribution; catches GC outliers) and an amortised
;; mean_ns / ops_per_sec from one bulk timer:tc over all N (per-call timer:tc
;; floors sub-microsecond ops at its ~1 us resolution, so the bulk mean is the
;; honest sub-us figure).
(defmodule uth.mt.bench-tests
  (export (run 0) (run 1)
          (nif-call-floor 1)
          (encode-atomic 1)
          (encode-composite 1)
          (decode-throughput 1)
          (resource-alloc 1)
          (lfe-baseline 1)))

(defun loop-n (n f)
  (lists:foreach (lambda (_) (funcall f)) (lists:seq 1 n)))

(defun pct (sorted p)
  (let* ((len (length sorted))
         (idx (max 1 (min len (round (* (/ p 100.0) len))))))
    (lists:nth idx sorted)))

(defun stats (n f)
  (let* ((samples (lists:sort (lists:map
                                (lambda (_) (element 1 (timer:tc f)))
                                (lists:seq 1 n))))
         (bulk-us (element 1 (timer:tc (lambda () (loop-n n f)))))
         (mean-ns (/ (* bulk-us 1000) n))
         (ops (if (> bulk-us 0) (round (/ n (/ bulk-us 1000000.0))) 'inf)))
    (map 'n n
         'median_us (pct samples 50)
         'p95_us (pct samples 95)
         'mean_ns mean-ns
         'ops_per_sec ops)))

;; 1. NIF call latency floor: a no-op NIF that just returns 'pong.
(defun nif-call-floor (n)
  (stats n (lambda () (uth.mt.nif:ping))))

;; 2. Term encoding (atomic): parse a pitch string -> integer, via the wrapper.
(defun encode-atomic (n)
  (stats n (lambda () (uth.mt.note:parse-midi-pitch "C4"))))

;; 3. Term encoding (composite): build a chord -> list of note maps.
(defun encode-composite (n)
  (stats n (lambda () (uth.mt.chord:notes 'C 'major 'triad))))

;; 4. Term decoding: raw NIF with a binary input (no LFE wrapper shaping) --
;;    isolates the NIF boundary; differs from (2) only by the wrapper's
;;    iolist_to_binary coercion.
(defun decode-throughput (n)
  (stats n (lambda () (uth.mt.nif:parse-midi-pitch #"F#3"))))

;; 5. ResourceArc allocation cost: uncached make-base-space (NOT
;;    uth.mt.oth:base-space, which caches). Each call is ~20 ms, so N is capped.
(defun resource-alloc (n)
  (stats (min n 200) (lambda () (uth.mt.nif:make-base-space))))

;; 6. Pure-LFE function-call anchor: identity in a tight loop.
(defun lfe-baseline (n)
  (stats n (lambda () (ident 'x))))

(defun ident (x) x)

(defun run () (run 10000))

(defun run (n)
  (map 'nif_call_floor (nif-call-floor n)
       'encode_atomic (encode-atomic n)
       'encode_composite (encode-composite n)
       'decode_throughput (decode-throughput n)
       'resource_alloc (resource-alloc n)
       'lfe_baseline (lfe-baseline n)))
