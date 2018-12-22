;;;;; TimePack version 2.0, Spring 2004
;;;;; an OpenMusic library for creating and manipulating rhythms

;;;;; Version 2.5 (only changing a function name: mrhythm in 2.0 = m-rhythm in 2.5)

;;;;; conceived and programmed by Paul Nauert,
;;;;; University of California, Santa Cruz

; in this file
; M-RHYTHM
#|
ABOUT OPTIMIZED PARAMETERS
For the sake of speed, probability tables and pools are converted internally
from lists to special array structures: 1-dim tables and pools are converted
to simple vectors, and larger-dim tables are converted to arrays of simple
vectors. This last structure is identified in certain function and variable
names as 'a/r' (for 'array of rows').
|#

;==================================================================================
;==================================================================================
; GLOBAL CONSTANTS

(defconstant *max-ptable-rank* 3)
; the highest allowed order for Markov processes is one less than this

(defconstant *max-pool-size* 64)
; the maximum allowed number of durations/duration-patterns in the pool

;;;;; icons
(defconstant *tp_icon* 332)
(defconstant *tp_util_icon* 333)
(defconstant *resize_icon* 334)
(defconstant *thru_icon* 335)

(defvar *target-distance*)
; point within this distance of a target is considered to coincide with the target
; initialized within M-RHYTHM (initial value defaults there to 0)

(setf *random-state* (make-random-state t))     ; unique randoms per session

;==================================================================================
;==================================================================================
; M-RHYTHM

(defmethod! M-RHYTHM (
                     pool ptable tot-time init-conds
                     &optional pattern-boundary-mode
                     target-points target-hit-weight
                     target-distance
                     )
  :icon *tp_icon*
  :doc "
Generates a rhythm in the form of a list of durations. Operates by repeatedly
selecting a duration or duration-pattern at random from POOL and appending it
to the result until a total duration of at least TOT-TIME is reached. Random
selections are made according to a Markov process of low order specified in
PTABLE. The context at the beginning of the process is determined by INIT-CONDS.

The first output is the resulting rhythm. The second output parses the resulting
rhythm into its constituent patterns. The third output reports the context at
the end of the process.

POOL is a length-M list of durations and duration-patterns; duration-patterns
are lists of durations. A duration is encoded as a positive number if it is a
note and a negative number if it is a rest. POOL can also have a special format
to allow bpf-driven interpolation between two contrasting length-M lists; see
'INTERPOLATION' below.

PTABLE is a flat or nested list:
- If the Markov process is of order 0, PTABLE is a length-M list specifying the
fixed probability of each item in the POOL.
- If the Markov process is of order 1, PTABLE is a length-M list of length-M
lists representing an M x M probability table.
- If the Markov process is of order greater than 1, PTABLE is a more deeply
nested list representing an M x M x ... x M probability table.
- PTABLE can also have a special format to allow bpf-driven interpolation
between two contrasting tables; see 'INTERPOLATION' below.

INTERPOLATION
POOL and PTABLE can be structured in the form (BPF START MIN MAX), where BPF
is an OpenMusic breakpoint function, START indicates what x-value of the BPF
corresponds to the start time of the resulting rhythm, MIN is the POOL or
PTABLE to use when BPF is at its minimum value, and MAX is the POOL or PTABLE
to use when BPF is at its maximum value. When BPF is at other values between
its minimum and maximum, the effective POOL or PTABLE is determined by
interpolation between MIN and MAX.

PATTERN BOUNDARIES
- If PATTERN-BOUNDARY-MODE is set to 'NOTE then the pattern (A B C) will lead
to the number |A| + |B| + |C| as an element of the second output.
- If the setting is 'NOTE+RESTS then the pattern (A B C) will lead instead to
the list of three numbers (|A| -|B| -|C|) as an element of the second output.

TARGET-BASED WEIGHTING
The optional TARGET-POINTS and TARGET-HIT-WEIGHT parameters provide (limited)
control over coincidences between the resulting rhythm and a predefined set of
timepoints. TARGET-POINTS specifies these timepoints; it is a list in which
each sublist is either a list of timepoints or a special list of the form
(:PERIODIC period begin end). At each step, M-RHYTHM tests every duration and
duration-pattern in POOL; each time it finds an element whose endpoint would
coincide with a timepoint in TARGET-POINTS, it multiplies the probability of
that element by TARGET-HIT-WEIGHT. Thus a TARGET-HIT-WEIGHT value greater than
1 favors coincidences with TARGET-POINTS, a value less than 1 disfavors them.
Normally, points must be exactly aligned to account as a coincidence, but if
the optional parameter TARGET-DISTANCE is set to a value above its default of
0, then points within this distance of one another count as coincidences."
  :indoc '("list of durations and patterns (pattern = list of durations)"
           "probability table (flat or nested list) controls random process"
           "target duration of output rhythm"
           "context for first few random choices"
           "format of second output"
           "control coincidences with these timepoints"
           "favor (> 1) or disfavor (< 1) coincidences with <target-points>"
           "maximum distance from target to count as coincidence")
  :menuins '((4 (("NOTE" 'NOTE) ("NOTE+RESTS" 'NOTE+RESTS))))
  :initvals '(nil nil 0 nil 'NOTE nil nil 0)
  :numouts 3
  
  ; tolerate lazy parameter construction
  (when (numberp init-conds) (setf init-conds (list init-conds)))
  (if (bpf-p (first pool))
    (setf (third pool) (durs->durlists (third pool))
          (fourth pool) (durs->durlists (fourth pool)))
    (setf pool (durs->durlists pool)))
  (setf *target-distance* (if (and (numberp target-distance)
                                   (> target-distance 0))
                            target-distance
                            0))
  
  ; check parameters
  (let ((bad-params
         (check-mrhythm-params pool ptable tot-time init-conds)))
    (if bad-params (throw-cancel (message-dialog bad-params))))
  
  ; optimize parameters
  (setf pool (if (bpf-p (first pool))
               (list (first pool) (second pool)
                     (coerce (third pool) 'vector)
                     (coerce (fourth pool) 'vector))
               (coerce pool 'vector))
        ptable (if (bpf-p (first ptable))
                 (list (first ptable) (second ptable)
                       (normalize-rows (tree->a/r (third ptable)))
                       (normalize-rows (tree->a/r (fourth ptable))))
                 (normalize-rows (tree->a/r ptable)))
        init-conds (let ((init-cond-count (length init-conds)))
                     (if (< init-cond-count (1- *max-ptable-rank*))
                       (append (make-list (- (1- *max-ptable-rank*) init-cond-count)
                                          :initial-element 0)
                               init-conds)
                       (nthcdr (- init-cond-count (1- *max-ptable-rank*))
                               init-conds))))
  
  (if (and target-points target-hit-weight)
    (do-mrhythm+target pool ptable tot-time init-conds
                       (equal pattern-boundary-mode 'NOTE+RESTS)
                       target-points target-hit-weight)
    (do-mrhythm pool ptable tot-time init-conds
                (equal pattern-boundary-mode 'NOTE+RESTS))))

;==================================================================================
; DO-MRHYTHM

(defun do-mrhythm (pool ptable tot-time init-conds note+rests?)
  (do ((elapsed-time 0)
       (LDUR nil) (PATTERN-BOUNDARIES nil) (CONTEXT init-conds) ; initialize return values
       (order (table-order ptable)))
      ((>= elapsed-time tot-time)
       (values LDUR PATTERN-BOUNDARIES CONTEXT))
    (let* ((p-row (fetch-row ptable context order elapsed-time))
           (new-pattern-index (get-random-index p-row))
           (new-pattern-list (fetch-pattern pool new-pattern-index elapsed-time))
           (new-pattern-dur (reduce #'+ new-pattern-list :key #'abs))
           (new-pattern-boundary-list
            (if note+rests?
              (cons (abs (first new-pattern-list))
                    (mapcar #'- (mapcar #'abs (rest new-pattern-list))))
              (list new-pattern-dur))))
      (incf elapsed-time new-pattern-dur)
      (setf LDUR (append LDUR new-pattern-list)
            PATTERN-BOUNDARIES (append PATTERN-BOUNDARIES new-pattern-boundary-list)
            CONTEXT (append (cdr CONTEXT) (list new-pattern-index)))
      )))

;==================================================================================
; DO-MRHYTHM+TARGET

(defun do-mrhythm+target (
                          pool ptable tot-time init-conds note+rests?
                          target-points target-hit-weight
                          )
  (do ((elapsed-time 0)
       (LDUR nil) (PATTERN-BOUNDARIES nil) (CONTEXT init-conds) ; initialize return values
       (order (table-order ptable)))
      ((>= elapsed-time tot-time)
       (values LDUR PATTERN-BOUNDARIES CONTEXT))
    (let* ((p-row (fetch-row ptable context order elapsed-time))
           (all-patterns (fetch-all-patterns pool elapsed-time))
           (all-pat-durs (mapcar (lambda (pat)
                                   (reduce #'+ pat :key #'abs))
                                 all-patterns))
           (targeted-p-row (make-targeted-p-row p-row target-points target-hit-weight
                                                all-pat-durs elapsed-time))
           (new-pattern-index (get-random-index targeted-p-row))
           (new-pattern-list (nth new-pattern-index all-patterns))
           (new-pattern-dur (nth new-pattern-index all-pat-durs))
           (new-pattern-boundary-list
            (if note+rests?
              (cons (abs (first new-pattern-list))
                    (mapcar #'- (mapcar #'abs (rest new-pattern-list))))
              (list new-pattern-dur))))
      (incf elapsed-time new-pattern-dur)
      (setf LDUR (append LDUR new-pattern-list)
            PATTERN-BOUNDARIES (append PATTERN-BOUNDARIES new-pattern-boundary-list)
            CONTEXT (append (cdr CONTEXT) (list new-pattern-index)))
      )))

;==================================================================================
;==================================================================================
; AUX
;==================================================================================
; - INPUT CHECKING

(defun check-mrhythm-params (pool ptable tot-time init-conds)
  "if bad param, returns string describing problem, else returns nil"
  (let* ((pool1 (if (bpf-p (first pool)) (third pool) pool))
         (pool2 (when (bpf-p (first pool)) (fourth pool)))
         (pool-size (length pool1))
         (ptable1 (if (bpf-p (first ptable)) (third ptable) ptable))
         (ptable2 (when (bpf-p (first ptable)) (fourth ptable))))
    (cond ((and pool2 (not (same-shape? pool1 pool2)))
           "M-RHYTHM: pools mismatched for interpolation")
          ((and ptable2 (not (well-formed-ptable? ptable1)))
           "M-RHYTHM: ill-formed first ptable")
          ((and ptable2 (not (well-formed-ptable? ptable2)))
           "M-RHYTHM: ill-formed second ptable")
          ((not (well-formed-ptable? ptable1))
           "M-RHYTHM: ill-formed ptable")
          ((or (and ptable2 (/= pool-size (length ptable2)))
               (/= pool-size (length ptable1)))
           "M-RHYTHM: pool and ptable(s) of incompatible sizes")
          ((not (plusp tot-time))
           "M-RHYTHM: tot-time not a positive number")
          ((and init-conds (>= (apply #'max init-conds) pool-size))
           "M-RHYTHM: init-conds out of range of pool")
          )))

(defun well-formed-ptable? (tree)
  "is tree a well-formed ptable?"
  (and (balanced? tree (length tree))
       (<= (leftmost-depth tree) *max-ptable-rank*)
       (<= (length tree) *max-pool-size*)))

(defun balanced? (tree target-length)   ; fixed in v. 2.5
     (if (flat? tree)
       (= (length tree) target-length)
       (and (balanced? (first tree) target-length)
           (multi-bool #'same-shape? tree))))

(defun same-shape? (tree1 tree2)
  (tree-equal tree1 tree2
              :test (lambda (x y)
                      (equal (atom x) (atom y)))))

(defun flat? (lst)
  (or (null lst)
      (and (atom (car lst))
           (flat? (cdr lst)))))

(defun leftmost-depth (tree)
  (if (atom tree) 0
      (1+ (leftmost-depth (car tree)))))

(defun multi-bool (bool-func arg-list)
  "(multi-bool #'bf '(a b c)) ==> (and (bf a b) (bf b c))"
  (let ((arg-count (length arg-list)))
    (cond ((< arg-count 2) nil)
          ((= arg-count 2) (apply bool-func arg-list))
          (t (and (funcall bool-func (first arg-list) (second arg-list))
                  (multi-bool bool-func (rest arg-list)))))))

;==================================================================================
; - M-RHYTHM AUX

(defun durs->durlists (pool)
  "if single-element pattern is input as atom, standardize by enclosing in list"
  (mapcar (lambda (pat) (if (listp pat) pat (list pat)))
          pool))

(defun tree->a/r (tree)
  "In: tree representing NxNx...xN table. Out: array-of-rows representing same."
  (if (flat? tree)
    (apply #'vector tree)
    (let* ((row-len (length tree))
           (a/r-dims (make-list (1- (leftmost-depth tree)) :initial-element row-len))
           (source (make-list-of-vectors tree))
           (result (make-array a/r-dims)))
      (dotimes (n (apply #'* a/r-dims))
        (setf (row-major-aref result n)
              (nth n source)))
      result)))

(defun make-list-of-vectors (tree)
  (if (null tree)
    nil
    (if (flat? (first tree))
      (cons (apply #'vector (first tree))
            (make-list-of-vectors (rest tree)))
      (append (make-list-of-vectors (first tree))
              (make-list-of-vectors (rest tree))))))

(defun normalize-rows (a/r)
  (if (numberp (row-major-aref a/r 0))
    (norm-row a/r)
    (let* ((dims (array-dimensions a/r))
           (result (make-array dims)))
      (dotimes (x (apply #'* dims))
        (setf (row-major-aref result x)
              (norm-row (row-major-aref a/r x))))
      result)))

(defun norm-row (vec)
  "In: simple vector. Out: same, scaled to sum to 1."
  (let ((sum (reduce #'+ vec)))
    (if (zerop sum)
      (let ((len (length vec)))
        (make-array len :initial-element (/ len)))
      (map 'simple-vector (lambda (x) (/ x sum))
           vec))))

;==================================================================================
; - DO-MRHYTHM AUX

(defun table-order (ptable)
  "
returns the order of ptable, which may a simple vector or an array of simple vectors; or if
ptable is a pair of ptables configured for bpf-driven interpolation, then returns a list of
the orders of the two tables"
  (cond ((listp ptable) (list (table-order (third ptable))
                              (table-order (fourth ptable))))
        ((a/r? ptable) (array-rank ptable))
        (t (1- (array-rank ptable)))))

(defun a/r? (obj)
  "is argument an array of rows (i.e. of simple vectors)?"
  (and (arrayp obj)
       (simple-vector-p (row-major-aref obj 0))))

(defun get-random-index (row)           ; assume row normalized to sum to 1
  (do* ((rand (random 1.0))
        (index 0 (1+ index))
        (accum (svref row 0) (+ accum (svref row index))))
       ((> accum rand) index)))

(defun fetch-row (table context order elapsed-time)
  (let* ((row-max (and (listp table)
                       (if (zerop (second order))
                         (fourth table)
                         (apply #'aref (fourth table) (trim-context context (second order))))))
         (table-min (if row-max (third table) table))
         (order-min (if (listp order) (first order) order))
         (row-min (if (zerop order-min)
                    table-min
                    (apply #'aref table-min (trim-context context order-min)))))
    (if row-max
      (make-interpolation row-min row-max elapsed-time
                          (first table) (second table) 'simple-vector)
      row-min)))

(defun trim-context (context order)
  "returns context trimmed to length n if order is n"
  (let ((surplus (- *max-ptable-rank* 1 order)))
    (nthcdr surplus context)))

(defun make-interpolation (row-min row-max x-val bpf x-offset result-type)
  (let* ((bpf-range (give-bpf-range bpf))
         (bpf-min (third bpf-range)) (bpf-max (fourth bpf-range))
         (bpf-samp (get-one-sample bpf (+ x-val x-offset)))
         (interp-factor (and bpf-samp (> bpf-max bpf-min)
                             (/ (- bpf-samp bpf-min)
                                (- bpf-max bpf-min)))))
    (if interp-factor
      (map result-type
           (lambda (mi ma) (+ (* ma interp-factor)
                              (* mi (- 1 interp-factor))))
           row-min row-max)
      row-min)))

(defun fetch-pattern (pool index elapsed-time)
  (let* ((pattern-max (and (listp pool)
                           (svref (fourth pool) index)))
         (pattern-min (if pattern-max
                        (svref (third pool) index)
                        (svref pool index))))
    (if pattern-max
      (make-interpolation pattern-min pattern-max elapsed-time
                          (first pool) (second pool) 'list)
      pattern-min)))

(defun get-one-sample (bpf x-val)
  "Returns the y-val when BPF is sampled at X-VAL, or NIL if X-VAL out of range."
  (first (bpf-sample bpf x-val x-val 2)))

;==================================================================================
; TARGET-HANDLING

; target-points is list of subtargets
; each subtarget is flat list of timepoints
; or special list (:PERIODIC period begin end)

(defun fetch-all-patterns (pool elapsed-time)
  (let ((poolsize (length pool)))
    (loop for i from 0 to (1- poolsize)
          collect (fetch-pattern pool i elapsed-time))))

(defun make-targeted-p-row (
                            p-row target-points target-hit-weight
                            all-pat-durs elapsed-time
                            )
  (let* ((all-pat-ends (om+ all-pat-durs elapsed-time))
         (prelim-list
          (loop for endpoint in all-pat-ends
                for prob across p-row
                collect (if (on-target? endpoint target-points)
                          (* prob target-hit-weight)
                          prob)))
         (prelim-vec (coerce prelim-list 'vector)))
    (norm-row prelim-vec)))

(defun on-target? (timepoint target-points)
  (loop for subtarget in target-points
        thereis (on-subtarget? timepoint subtarget)))

(defun on-subtarget? (timepoint subtarget)
  (cond ((numberp (first subtarget))
         (member timepoint subtarget
                 :test #'within-target-distance))
        ((and (equal (first subtarget) :PERIODIC)
              (= (length subtarget) 4))
         (let ((period (second subtarget))
               (begin (third subtarget))
               (end (fourth subtarget)))
           (and (<= begin timepoint end)
                (zerop (nth-value 1 (floor (- timepoint begin) period))))))))

(defun within-target-distance (x y)
  (if (zerop *target-distance*)
    (= x y)
    (<= (abs (- x y)) *target-distance*)))

;==================================================================================
;==================================================================================
#|
 E X A M P L E S
(Because M-RHYTHM implements a random process, repeated calls with the same parameters
will typically produce different return values.)

> (M-RHYTHM '(20 35 (5 50) (5 80)) '(5 3 2 1) 800 NIL) 
 M-RHYTHM returned 3 values :
      (35 20 35 20 5 80 35 35 35 20 5 50 5 50 20 20 20 20 5 50 35 20 35 20 20 35 20 35 20)
      (35 20 35 20 85 35 35 35 20 55 55 20 20 20 20 55 35 20 35 20 20 35 20 35 20)
      (1 0)

> (M-RHYTHM '(20 35 (5 50) (5 80)) '(5 3 2 1) 800 NIL 'NOTE+RESTS) 
 M-RHYTHM returned 3 values :
      (5 80 5 50 20 20 20 20 20 35 20 5 80 35 20 35 20 5 80 35 35 20 20 5 50 5 50 20)
      (5 -80 5 -50 20 20 20 20 20 35 20 5 -80 35 20 35 20 5 -80 35 35 20 20 5 -50 5 -50 20)
      (2 0)

> (M-RHYTHM '(20 35 (-5 50) (-5 80))
           '((3 1 0 0)
             (1 3 1 1)
             (2 3 1 0)
             (2 3 0 0))
           800 '(0) 'NOTE+RESTS) 
 M-RHYTHM returned 3 values :
      (35 -5 80 35 35 -5 50 35 35 35 -5 50 35 35 -5 50 35 35 35 -5 50 20 20 35 -5 50)
      (35 5 -80 35 35 5 -50 35 35 35 5 -50 35 35 5 -50 35 35 35 5 -50 20 20 35 5 -50)
      (1 2)

(setf MY-BPF (simple-bpf-from-list '(0 400 800) '(0 100 0)))

> (M-RHYTHM '(20 35 (-5 50) (-5 80)) 
           (list MY-BPF 0
                 '((3 1 0 0) (1 3 1 1) (2 3 1 0) (2 3 0 0))
                 '(0 2 1 0))
           800 '(1))
 M-RHYTHM returned 3 values :
      (35 -5 80 20 -5 50 35 35 35 35 -5 50 -5 50 35 35 35 35 35 35 35 20 35 20 20 20)
      (35 85 20 55 35 35 35 35 55 55 35 35 35 35 35 35 35 20 35 20 20 20)
      (0 0)
|#

;;;;; END
