(undermidi:start)
(progn
  (um:set-device 0)
  (um:set-channel 0)
  'ok)

(set scale-name 'aeolian)
(set scale-template (mref (uth.scale:all) scale-name))
(set scale (uth.pitch:template-> scale-template))

(set model (uth.melody:new-model scale-name (uth.melody:default-model)))
(set m (uth.melody:random-walk 'aeolian))

(defun play-melody
  ((scale-name `#m(octave ,octave vel ,vel dur ,dur))
    (let* ((overrides #m(bars 4))
           (model (uth.melody:new-model scale-name))
           (melody (uth.melody:make scale-name overrides model)))
      (io:format "Scale info:\n  ~p\n" (list (mref model 'scale)))
      (io:format "Generated melody:\n  ~p\n" (list melody))
      (list-comp
        ((<- pitch melody))
        (um.note:play (+ (* octave 12) pitch) vel dur)))
    'ok))

(set opts `#m(
  octave 4
  vel 50
  dur 750))

(play-melody 'aeolian opts)

(list-comp
  ((<- pitch (uth.melody:make scale-name #m(bars 4))))
  (um.note:play (+ (* octave 12) pitch) vel dur))

(list-comp
  ((<- pitch (uth.melody:make scale-name)))
  (um.note:play (+ (* octave 12) pitch) vel dur))

(list-comp
  ((<- pitch (uth.melody:random-walk scale-name)))
  (um.note:play (+ (* octave 12) pitch) vel dur))

(list-comp
  ((<- pitch scale))
  (um.note:play (+ (* octave 12) pitch) vel dur))

(set s '(0 2 3 5 7 8 10 12 14 15))
(set m '(12 10 12 10 8 10 8 7 5 3 2 0 2 3 2 5))
(lists:reverse m)
(set rm '(5 2 3 2 0 2 3 5 7 8 10 8 10 12 10 12))
(set orm '(0 -3 -2 -3 -5 -3 -2 0 2 3 5 3 5 7 5 7))
(set iorm '(0 3 2 3 5 3 2 0 -2 -3 -5 -3 -5 -7 -5 -7))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch m))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

(defun offsets
  (((= `(,first . ,_)))
   (list-comp
     ((<- x scale))
     (- x first))))

(defun inverse-offsets (scale melody)
  (list-comp
    ((<- x (offsets scale melody)))
    (* x -1)))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip melody (offsets melody))))
    (let ((note (+ (* octave 12) (+ pitch 15)))
          (inote (+ (* octave 12) (+ ipitch 15))))
      (um.chord:play (list note inote) vel dur))))

(set +iorm (list-comp ((<- x iorm)) (+ 12 x)))
(set ds (lists:uniq (lists:append s (list-comp ((<- x s)) (+ 12 x)))))
(set iorm2 (list-comp ((<- x +iorm)) (lists:nth x ds)))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip rm iorm2)))
    (let ((note (+ (* octave 12) (+ pitch 15)))
          (inote (+ (* octave 12) (+ ipitch 15))))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur))))))

(transpose (inverse-offsets melody) 10)

(defun play-melody
  ((scale-name `#m(octave ,octave vel ,vel dur ,dur))
    (let* ((overrides #m(bars 4))
           (model (uth.melody:new-model scale-name))
           (melody (uth.melody:make scale-name overrides model)))
      (io:format "Scale info:\n  ~p\n" (list (mref model 'scale)))
      (io:format "Generated melody:\n  ~p\n" (list melody))
      (list-comp
        ((<- pitch melody))
        (um.note:play (+ (* octave 12) pitch) vel dur))
    'ok)))

(defun play-melodies
  ((scale-name `#m(octave ,octave vel ,vel dur ,dur))
   (let* ((overrides #m(bars 4))
          (model (uth.melody:new-model scale-name))
          (scale (mref model 'scale))
          (melody (uth.melody:make scale-name overrides model))
          (reversed (reverse scale melody)))
     (io:format "Scale info:\n  ~p\n" (list (mref model 'scale)))
     (io:format "Generated melody:\n  ~p\n" (list melody))
     (io:format "Reversed melody:\n  ~p\n" (list reversed))
     (list-comp
         ((<- `#(,pitch ,rpitch) (lists:zip melody reversed)))
       (let ((note (+ (* octave 12) (+ pitch 15)))
             (rnote (+ (* octave 12) (+ rpitch 15))))
         (case (== rnote note)
           ('true (um.note:play note vel dur))
           (_ (um.chord:play (list note rnote) vel dur)))))
     'ok)))

;; ---------------------------------------------------------

(set s '(0 2 3 5 7 8 10 12 14 15))
(set rm '(12 10 12 10 8 10 8 7 5 3 2 0 2 3 2 5))
(lists:reverse rm)
(set m '(5 2 3 2 0 2 3 5 7 8 10 8 10 12 10 12))
(set iorm '(0 -3 -2 -3 -5 -3 -2 0 2 3 5 3 5 7 5 7))
(set orm '(0 3 2 3 5 3 2 0 -2 -3 -5 -3 -5 -7 -5 -7))
(set +orm (transpose orm 12))
(set ds (extend-scale s 2))
(set orm2 (list-comp ((<- x +orm)) (lists:nth x ds)))

(set orm2 (transpose (lists:map (lambda (x) (lists:nth x (extend-scale s 2))) (transpose (inverse-offsets (offsets melody-indices)) 12)) -12))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip m orm2)))
    (let ((note (+ (* octave 12) (+ pitch 15)))
          (inote (+ (* (- octave 1) 12) (+ ipitch 15))))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur)))))
  'ok)

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch m))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch orm2))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

(defun offsets
  "This generates a list of offsets, relative to the first note of a melody."
  (((= `(,first . ,_) melody))
   (list-comp
     ((<- x melody))
     (- x first))))

(defun inverse-offsets (melody)
  (list-comp
    ((<- x (offsets melody)))
    (* x -1)))

(defun transpose (melody offset)
  (list-comp
    ((<- x melody))
    (+ offset x)))

(defun extend-scale (scale times)
  "This function takes a scale and extends it by the given amount.

  Note that this creates duplicate entries (due to overlap), so these are
  removed by uniquing this result."
  (lists:sort
    (lists:uniq
      (list-comp
        ((<- x scale)
         (<- y (lists:seq 0 (- times 1))))
        (+ (* y 12) x)))))

;; -----------------------------------------------------------------------

(defun play-melodies
  ((scale-name `#m(octave ,octave vel ,vel dur ,dur))
   (let* ((overrides #m(bars 4))
          (scale (mref (uth.scale:all) scale-name))
          (model (uth.melody:new-model scale-name))
          (melody (uth.melody:make scale-name overrides model))
          (reversed (reverse scale melody)))
     (io:format "Scale info:\n  ~p\n" (list (mref model 'scale)))
     (io:format "Generated melody:\n  ~p\n" (list melody))
     (io:format "Reversed melody:\n  ~p\n" (list reversed))
     (list-comp
         ((<- `#(,pitch ,rpitch) (lists:zip melody reversed)))
       (let ((note (+ (* octave 12) (+ pitch 15)))
             (rnote (+ (* octave 12) (+ rpitch 15))))
         (case (== rnote note)
           ('true (um.note:play note vel dur))
           (_ (um.chord:play (list note rnote) vel dur)))))
     'ok)))

(set lookup
     (maps:from_list
      (lists:foldl
       (match-lambda
         ((x '())
          `(#(,x 1)))
         ((x (= `(#(,_ ,last) . ,_) acc))
          (lists:append `(#(,x ,(+ 1 last))) acc)))
       '()
       (extend-scale s 3))))

(set orm2
     (list-comp
         ((<- x (inverse-offsets (offsets (list-comp
                                            ((<- p m))
                                            (mref lookup p))))))
       (+ x (car m))))

(set melody-indices (list-comp ((<- p m)) (mref lookup p)))

(list-comp ((<- x (inverse-offsets (offsets (list-comp ((<- p m)) (mref lookup p)))))) (+ x (car m)))
(inverse-offsets (offsets (list-comp ((<- p m)) (mref lookup p))))
(offsets (list-comp ((<- p m)) (mref lookup p)))


(set orm2
  (transpose
    (lists:map
      (lambda (x)
        (lists:nth x (extend-scale s 2)))
      (transpose
       (inverse-offsets (offsets melody-indices))
       12))
    -12))

m
(5 2 3 2 0 2 3 5 7 8 10 8 10 12 10 12)

(set melody-indices (list-comp ((<- p m)) (mref lookup p)))
(4 2 3 2 1 2 3 4 5 6 7 6 7 8 7 8)
(offsets melody-indices)
(0 -2 -1 -2 -3 -2 -1 0 1 2 3 2 3 4 3 4)
(set rel-inv-offsets (inverse-offsets (offsets melody-indices)))
(0 2 1 2 3 2 1 0 -1 -2 -3 -2 -3 -4 -3 -4)

(set inv-offsets (list-comp ((<- x rel-inv-offsets)) (+ (car melody-indices) x)))

(set t-val (get-transpose-val (lists:min inv-offs)))
12
(set trans-offs (transpose inv-offs t-val))
(12 14 13 14 15 14 13 12 11 10 9 10 9 8 9 8)

(set ext-scale (extend-scale s (get-scale-val (lists:max trans-offs))))
(0 2 3 5 7 8 10 12 14 15 17 19 20 22 24 26 27)

(set unadj-inverted-melody (lists:map (lambda (x) (lists:nth x ext-scale)) trans-offs))
(19 22 20 22 24 22 20 19 17 15 14 15 14 12 14 12)
(set interted-melody (transpose unadj-inverted-melody -12))
(7 10 8 10 12 10 8 7 5 3 2 3 2 0 2 0)

(set melody-indices (list-comp ((<- p m)) (mref lookup p)))
(set inv-offs (inverse-offsets (offsets melody-indices)))
(set t-val (get-transpose-val (lists:min inv-offs)))
(set trans-offs (transpose inv-offs t-val))
(set ext-scale (extend-scale s (get-scale-val (lists:max trans-offs))))
(set unadj-inverted-melody (lists:map (lambda (x) (lists:nth x ext-scale)) trans-offs))
(set interted-melody (transpose unadj-inverted-melody -12))

;; -----------------------------------------------------------------------

(set melody-indices (list-comp ((<- p m)) (mref lookup p)))
(4 2 3 2 1 2 3 4 5 6 7 6 7 8 7 8)
(offsets melody-indices)
(0 -2 -1 -2 -3 -2 -1 0 1 2 3 2 3 4 3 4)
(set rel-inv-offsets (inverse-offsets (offsets melody-indices)))
(0 2 1 2 3 2 1 0 -1 -2 -3 -2 -3 -4 -3 -4)

(set inv-offsets (list-comp ((<- x rel-inv-offsets)) (+ (car melody-indices) x)))
(4 6 5 6 7 6 5 4 3 2 1 2 1 0 1 0)

ext-scale
(0 2 3 5 7 8 10 12 14 15 17 19 20 22 24 26 27 29 31 32 34 36 38 39 41 43 44 46 48 50 ...)

(set unadj-inverted-melody (list-comp ((<- x inv-offsets)) (lists:nth (+ x (mref model 'base-scale-length)) ext-scale)))
(17 20 19 20 22 20 19 17 15 14 12 14 12 10 12 10)
(set inverted-melody (transpose unadj-inverted-melody -12))
(5 8 7 8 10 8 7 5 3 2 0 2 0 -2 0 -2)

(set inverted-melody (inverted-last-notes model inverted-melody))
(5 8 7 8 10 8 7 5 3 2 0 2 0 -2 0 -2 2 0)

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip m inverted-melody)))
    (let ((note (+ (* octave 12) (+ pitch 15)))
          (inote (+ (* (- octave 1) 12) (+ ipitch 15))))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur)))))
  'ok)

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch m))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch inverted-melody))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

;; -----------------------------------------------------------------------

(defun direction1 ()
  (round (* 2 (- (rand:uniform 2) 1.5))))

(defun direction2 ()
  (case (rand:uniform 2)
    (1 1)
    (2 -1)))

(defun time1 ()
  (lists:map (lambda (_) (direction1)) (lists:seq 0 1000000)))

(defun time2 ()
  (lists:map (lambda (_) (direction2)) (lists:seq 0 1000000)))

(timer:tc #'time1/0)
(timer:tc #'time2/0)

;; -----------------------------------------------------------------------

(defun direction ()
  (case (rand:uniform 2)
    (1 1)
    (2 -1)))

(defun random-walk (max)
  (random-walk max '()))

(defun random-walk
 ((max acc) (when (>= (length acc) max))
   acc)
 ((max acc)
   (random-walk max (lists:append acc (list (direction))))))

(random-walk 20)
(-1 -1 -1 1 -1 -1 1 1 -1 -1 -1 -1 -1 1 1 -1 -1 1 -1 -1)

(defun random-walk
  ((`#m(scale ,s) min max)
   (let ((max-count (uth.melody:base-note-count model))
         (scale (extend-scale s 3)))
     (list-comp ((<- x (lists:reverse (random-walk min max max-count '(0)))))
       (- (lists:nth (+ x 12) scale) 12)))))

(defun random-walk
 ((min max max-count acc) (when (>= (length acc) max))
   acc)
 ((min max max-count (= `(,prev . ,_) acc))
   (random-walk min
                max
                max-count
                (lists:append (list (next min max prev)) acc))))

(defun next
 ((min _ prev) (when (=< prev min))
  (+ prev 1))
 ((_ max prev) (when (>= prev max))
  (- prev 1))
 ((_ _ prev)
  (+ prev (direction))))

(random-walk model 0 10)

(set opts `#m(
  octave 4
  vel 50
  dur 750))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch (random-walk model 0 10)))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

;; -----------------------------------------------------------------------

