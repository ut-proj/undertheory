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
          (inote (+ (* octave 12) (+ ipitch 15))))
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

(undermidi@lappy.lan)lfe> m
(5 2 3 2 0 2 3 5 7 8 10 8 10 12 10 12)

(undermidi@lappy.lan)lfe> (set melody-indices (list-comp ((<- p m)) (mref lookup p)))
(4 2 3 2 1 2 3 4 5 6 7 6 7 8 7 8)
(undermidi@lappy.lan)lfe> (offsets melody-indices)
(0 -2 -1 -2 -3 -2 -1 0 1 2 3 2 3 4 3 4)
(undermidi@lappy.lan)lfe> (set inv-offs (inverse-offsets (offsets melody-indices)))
(0 2 1 2 3 2 1 0 -1 -2 -3 -2 -3 -4 -3 -4)

(undermidi@lappy.lan)lfe> (set t-val (get-transpose-val (lists:min inv-offs)))
12
(undermidi@lappy.lan)lfe> (set trans-offs (transpose inv-offs t-val))
(12 14 13 14 15 14 13 12 11 10 9 10 9 8 9 8)

(undermidi@lappy.lan)lfe> (set ext-scale (extend-scale s (get-scale-val (lists:max trans-offs))))
(0 2 3 5 7 8 10 12 14 15 17 19 20 22 24 26 27)

(undermidi@lappy.lan)lfe> (set unadj-inverted-melody (lists:map (lambda (x) (lists:nth x ext-scale)) trans-offs))
(19 22 20 22 24 22 20 19 17 15 14 15 14 12 14 12)
(undermidi@lappy.lan)lfe> (set interted-melody (transpose unadj-inverted-melody -12))
(7 10 8 10 12 10 8 7 5 3 2 3 2 0 2 0)

(set melody-indices (list-comp ((<- p m)) (mref lookup p)))
(set inv-offs (inverse-offsets (offsets melody-indices)))
(set t-val (get-transpose-val (lists:min inv-offs)))
(set trans-offs (transpose inv-offs t-val))
(set ext-scale (extend-scale s (get-scale-val (lists:max trans-offs))))
(set unadj-inverted-melody (lists:map (lambda (x) (lists:nth x ext-scale)) trans-offs))
(set interted-melody (transpose unadj-inverted-melody -12))
