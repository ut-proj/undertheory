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
  ((`#m(scale ,s min-interval ,min max-interval ,max generate-count ,gc))
   (let ((scale (extend-scale s 3)))
     (list-comp ((<- x (lists:reverse (random-walk min max gc '(0)))))
       (- (lists:nth (+ x 12) scale) 12)))))

(defun random-walk
 ((min max max-count acc) (when (>= (length acc) max-count))
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

(set model (uth.melody:new-model
             scale-name
             (mupd (uth.melody:default-model) 'bars 4)))

(set opts `#m(
  octave 4
  vel 50
  dur 750))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts))
  (list-comp
    ((<- pitch (random-walk model 0 10)))
    (um.note:play (+ (* octave 12) pitch) vel dur)))

;; -----------------------------------------------------------------------

(defun default-model ()
  `#m(generator-fn #'uth.melody:random-walk/1
      default-scale 'aeolian
      min-interval 1
      max-interval 10
      time-signature #(4 4)
      bars 2
      first-note-indices (1 3 5)
      majority-range 6
      majority-percent 0.6
      reverse-chance 0.5
      chance-for-2nd 0.5
      chance-for-3rd 0.2
      chance-for-4th 0.1
      chance-for-5th 0.1
      chance-for-6th 0.025
      chance-for-7th 0.025
      chance-for-8th 0.05
      chance-for-direction-change 0.25
      final-note-count 2))

(set model (uth.melody:new-model
             scale-name
             (mupd (default-model) 'bars 4)))
(defun run
 (((= `#m(generator-mf ,generator-mf) model))
  (call (lists:append generator-mf (list model)))))

(defun run
 (((= `#m(generator-fn ,generator-fn) model))
  (apply generator-fn (list model))))

(defun run
 (((= `#m(generator-fn ,generator-fn) model))
  (io:format "~p\n" (list generator-fn))
  (funcall generator-fn model)))

(undermidi:start)
(progn
  (um:set-device 0)
  (um:set-channel 0)
  'ok)

(set scale-name 'aeolian)

(set model (uth.melody:new-model
             scale-name
             (mupd (uth.melody:default-model) 'bars 4)))

(set opts `#m(
  octave 4
  vel 50
  dur 750))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts)
      (melody (uth.melody:random-walk model)))
  (lfe_io:format "\nGenerated melody: \n  ~p\n" (list melody))
  (lfe_io:format "Generated melody length: \n  ~p\\nn" (list (length melody)))
  (list-comp
    ((<- pitch melody))
    (um.note:play (+ (* octave 12) pitch) vel dur))
  'ok)

;; -----------------------------------------------------------------------

(defun random-walk
  (((= `#m(scale ,s min-interval ,min max-interval ,max base-count ,bc invert-chance ,ic) model))
   (let* ((steps (random-step min max bc '(0)))
          (adj (last-steps steps)))
      adj)))

(defun random-walk
  (((= `#m(scale ,s min-interval ,min max-interval ,max base-count ,bc invert-chance ,ic) model))
   (let* ((scale-mult (+ 2 (ceil (/ max 12))))
          (scale (extend-scale s scale-mult))
          (steps (random-step min max bc '(0))))
      steps)))

(defun random-walk
  (((= `#m(scale ,s min-interval ,min max-interval ,max base-count ,bc invert-chance ,ic) model))
   (let* ((steps (random-step min max bc '(0)))
          (adj (last-steps steps))
          (scale-mult (+ 2 (ceil (/ max 12))))
          (scale (extend-scale s scale-mult)))
      adj)))

(defun random-walk
  (((= `#m(scale ,s min-interval ,min max-interval ,max base-count ,bc invert-chance ,ic) model))
   (let* ((steps (random-step min max bc '(0)))
          (adj (last-steps steps))
          (adj-reversed (last-steps (lists:reverse adj)))
          (shifted (list-comp ((<- x adj)) (+ 1 x)))
          (shifted-reversed (list-comp ((<- x adj-reversed)) (+ 1 x)))
          (scale-mult (+ 2 (ceil (/ max 12))))
          (scale (extend-scale s scale-mult))
          (indexed (maps:from_list (lists:enumerate scale)))
          (pitches (list-comp ((<- x shifted)) (mref indexed (+ 12 x))))
          (pitches-reversed (list-comp ((<- x shifted-reversed)) (mref indexed (+ 12 x)))))
      `#m(steps ,adj
          steps-reversed ,adj-reversed
          shifted ,shifted
          shifted-reversed ,shifted-reversed
          scale ,scale
          indexed ,indexed
          pitches ,pitches
          pitches-reversed ,pitches-reversed))))

(defun random-walk
 ((min max max-count acc) (when (>= (length acc) max-count))
   (lists:reverse acc))
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

(defun first-notes
  ((`#m(first-note-indices ,possibles))
   (let* ((len (length possibles))
          (index (rand:uniform len)))
     `#m(first ,(list (lists:nth index possibles))
         index ,index))))

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts)
      (melody (mref results 'pitches)))
  (lfe_io:format "\nGenerated melody: \n  ~p\n" (list melody))
  (lfe_io:format "Generated melody length: \n  ~p\n\n" (list (length melody)))
  (list-comp
    ((<- pitch melody))
    (um.note:play (+ (* octave 12) pitch) vel dur))
  'ok)

(let ((`#m(octave ,octave vel ,vel dur ,dur) opts)
      (melody (mref results 'pitches))
      (melody-reversed (mref results 'pitches-reversed)))
  (lfe_io:format "\nGenerated melody: \n  ~p\n" (list melody))
  (lfe_io:format "Reversed melody: \n  ~p\n" (list melody-reversed))
  (lfe_io:format "Generated melody length: \n  ~p\n\n" (list (length melody)))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip melody melody-reversed)))
    (let ((note (+ (* octave 12) pitch))
          (inote (+ (* (- octave 1) 12) ipitch)))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur)))))
  'ok)

(let* ((`#m(octave ,octave vel ,vel dur ,dur) opts)
       (results (random-walk model))
       (melody (mref results 'pitches))
       (melody-reversed (mref results 'pitches-reversed)))
  (lfe_io:format "\nGenerated melody: \n  ~p\n" (list melody))
  (lfe_io:format "Reversed melody: \n  ~p\n" (list melody-reversed))
  (lfe_io:format "Generated melody length: \n  ~p\n\n" (list (length melody)))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip melody melody-reversed)))
    (let ((note (+ (* octave 12) pitch))
          (inote (+ (* (- octave 2) 12) ipitch)))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur)))))
  'ok)

;; -----------------------------------------------------------------------

(defun random-walk
  (((= `#m(scale ,s min-interval ,min max-interval ,max base-count ,bc invert-chance ,ic) model))
   (let* ((steps (random-step model '(0)))
          (adj (last-steps steps))
          (adj-reversed (last-steps (lists:reverse adj)))
          (shifted (list-comp ((<- x adj)) (+ 1 x)))
          (shifted-reversed (list-comp ((<- x adj-reversed)) (+ 1 x)))
          (scale-mult (+ 4 (ceil (/ max 12))))
          (scale (extend-scale s scale-mult))
          (indexed (maps:from_list (lists:enumerate scale)))
          (pitches (list-comp ((<- x shifted)) (mref indexed (+ 12 x))))
          (pitches-reversed (list-comp ((<- x shifted-reversed)) (mref indexed (+ 12 x)))))
      `#m(steps ,adj
          steps-reversed ,adj-reversed
          shifted ,shifted
          shifted-reversed ,shifted-reversed
          scale ,scale
          indexed ,indexed
          pitches ,pitches
          pitches-reversed ,pitches-reversed))))

(defun random-step
 (((= `#m(min-interval ,min max-interval ,max base-count ,bc) model) acc) (when (>= (length acc) bc))
   (lists:reverse acc))
 (((= `#m(min-interval ,min max-interval ,max base-count ,bc) model) (= `(,prev . ,_) acc))
   (random-step model
                (lists:append (list (* (next model prev) (direction model) )) acc))))

(defun next
 ((`#m(min-interval ,min) prev) (when (=< prev min))
  (+ prev 1))
 ((`#m(max-interval ,max) prev) (when (>= prev max))
  (- prev 1))
 ((model prev)
  (case (rand:uniform_real)
     (x (when (=< (mref model 'threshold-for-2nd) x)) (+ 1 prev))
     (x (when (=< (mref model 'threshold-for-3rd) x)) (+ 2 prev))
     (x (when (=< (mref model 'threshold-for-4th) x)) (+ 3 prev))
     (x (when (=< (mref model 'threshold-for-5th) x)) (+ 4 prev))
     (x (when (=< (mref model 'threshold-for-6th) x)) (+ 5 prev))
     (x (when (=< (mref model 'threshold-for-7th) x)) (+ 6 prev))
     (_ (+ 7 prev)))))

(defun direction
  ((`#m(chance-for-direction-change ,change-chance))
  (if (>= (rand:uniform_real) change-chance)
    1
    -1)))

(let* ((`#m(octave ,octave vel ,vel dur ,dur) opts)
       (results (random-walk model))
       (melody (mref results 'pitches))
       (melody-reversed (mref results 'pitches-reversed)))
  (lfe_io:format
    "\nGenerated & reversed melodies: \n  ~p\n  ~p\n"
    (list melody melody-reversed))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip melody melody-reversed)))
    (let ((note (+ (* octave 12) pitch))
          (inote (+ (* (- octave 2) 12) ipitch)))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur)))))
  'ok)

;; -----------------------------------------------------------------------

Nice Examples:

Generated melody:
  (20 22 24 22 24 26 27 26 27 29 31 32 31 32 32 34)
Reversed melody:
  (34 32 32 31 32 31 29 27 26 27 26 24 22 24 22 20)

Generated melody:
  (20 19 20 22 24 26 24 22 24 22 24 26 27 29 32 34)
Reversed melody:
  (34 32 29 27 26 24 22 24 22 24 26 24 22 20 22 20)
;; -----------------------------------------------------------------------

(set opts `#m(
  octave 3
  vel 50
  dur 500))

(defun random-walk
  (((= `#m(scale ,s min-interval ,min max-interval ,max base-count ,bc invert-chance ,ic) model))
   (let* ((steps (random-step model '(0)))
          (adj (last-steps (shift-up steps)))
          (adj-reversed (last-steps (lists:reverse adj)))
          (scale-mult (+ 12 (ceil (/ max 12))))
          (scale (extend-scale s scale-mult))
          (indexed-plist (lists:enumerate scale))
          (indexed (maps:from_list indexed-plist))
          (pitches (list-comp ((<- x adj)) (mref indexed x)))
          (pitches-reversed (list-comp ((<- x adj-reversed)) (mref indexed x))))
      `#m(steps ,steps
          adj ,adj
          adj-reversed ,adj-reversed
          scale ,scale
          indexed-plist ,indexed-plist
          pitches ,pitches
          pitches-reversed ,pitches-reversed))))

(random-walk model)

(defun random-step
 (((= `#m(min-interval ,min max-interval ,max base-count ,bc) model) acc) (when (>= (length acc) bc))
   (lists:reverse acc))
 (((= `#m(min-interval ,min max-interval ,max base-count ,bc) model) (= `(,prev . ,_) acc))
   (random-step model
                (lists:append (list (next model prev)) acc))))

(defun next
 ((`#m(min-interval ,min) prev) (when (=< prev min))
  (+ prev 1))
 ((`#m(max-interval ,max) prev) (when (>= prev max))
  (- prev 1))
 ((model prev)
  (let ((dir (direction model)))
   (case (rand:uniform_real)
      (x (when (=< (mref model 'threshold-for-2nd) x)) (+ (* 1 dir) prev))
      (x (when (=< (mref model 'threshold-for-3rd) x)) (+ (* 2 dir) prev))
      (x (when (=< (mref model 'threshold-for-4th) x)) (+ (* 3 dir) prev))
      (x (when (=< (mref model 'threshold-for-5th) x)) (+ (* 4 dir) prev))
      (x (when (=< (mref model 'threshold-for-6th) x)) (+ (* 5 dir) prev))
      (x (when (=< (mref model 'threshold-for-7th) x)) (+ (* 6 dir) prev))
      (_ (+ (* 7 dir) prev))))))

(let* ((`#m(octave ,octave vel ,vel dur ,dur) opts)
       (results (random-walk model))
       (melody (mref results 'pitches))
       (melody-reversed (mref results 'pitches-reversed)))
  (lfe_io:format
    "\nGenerated & reversed melodies: \n  ~p\n  ~p\n"
    (list melody melody-reversed))
  (list-comp
    ((<- `#(,pitch ,ipitch) (lists:zip melody melody-reversed)))
    (let ((note (+ (* octave 12) pitch))
          (inote (+ (* (- octave 2) 12) ipitch)))
      (case (== inote note)
        ('true (um.note:play note vel dur))
        (_ (um.chord:play (list note inote) vel dur)))))
  'ok)

(let* ((`#m(octave ,octave vel ,vel dur ,dur) opts)
       (results (random-walk model))
       (melody (mref results 'pitches)))
  (lfe_io:format "\nGenerated melody: \n  ~p\n" (list melody))
  (lfe_io:format "Generated melody length: \n  ~p\n\n" (list (length melody)))
  (list-comp
    ((<- pitch melody))
    (um.note:play (+ (* octave 12) pitch) vel dur))
  'ok)
