;;;;; TimePack version 2.5, Summer 2005
;;;;; an OpenMusic library for creating and manipulating rhythms

;;;;;  this file is an entirely new addition to OMTP in v 2.5

;;;;; conceived and programmed by Paul Nauert,
;;;;; University of California, Santa Cruz

; in this file
; M-MAPPED-RHYTHM
; M-EMPTY-BARS
; M-VARI-PULSE

;==================================================================================
;==================================================================================
; TIMEPACK GENERATOR FUNCTIONS
; M-MAPPED-RHYTHM randomly selects generic durations and maps them onto a variable
; pulse. Other functions in this file support construction of the variable pulse.
;==================================================================================

(in-package :om)

;==================================================================================
; M-MAPPED-RHYTHM

(defmethod! M-MAPPED-RHYTHM (
                             pool ptable vari-pulse tot-time init-conds
                             &optional (offset 0) pattern-boundary-mode
                             )
  :icon *tp_icon*
  :doc "
Randomly selects generic durations/patterns from POOL and maps them onto
the input VARI-PULSE to create a rhythm with a total duration of TOT-TIME.
Random selections are controlled by the probabilities in PTABLE. Mapping
starts from the beginning of VARI-PULSE, or with an optional OFFSET.

BPF-driven interpolation is possible on the PTABLE as in M-RHYTHM.

The first output is the resulting rhythm, the second delineates pattern
boundaries (as in M-RHYTHM), and the third reports the end context, which
may serve as initial conditions for the next in a cascaded series of
M-MAPPED-RHYTHM functions."
  :indoc '("list of generic durations and patterns (pattern = list of durations)"
           "probability table (flat or nested list) controls random process"
           "variable pulse onto which generic durations will be mapped"
           "target duration of output rhythm"
           "context for first few random choices"
           "start beyond beginning of vari-pulse"
           "format of second output")
  :menuins '((6 (("NOTE" 'NOTE) ("NOTE+RESTS" 'NOTE+RESTS))))
  :initvals '(nil nil nil 0 nil 0 'NOTE)
  :numouts 3
  
  ; tolerate lazy parameter construction
  (when (numberp init-conds) (setf init-conds (list init-conds)))
  (setf pool (durs->durlists pool))
  
  ; check parameters
  (let ((bad-params (check-mapping-params pool ptable tot-time init-conds)))
    (if bad-params (throw-cancel (message-dialog bad-params))))
  (let ((available-time (- (apply #'+ vari-pulse) offset)))
    (cond ((minusp available-time)
           (throw-cancel (message-dialog "M-MAPPED-RHYTHM: OFFSET exceeds time spanned by VARI-PULSE")))
          ((> tot-time available-time)
           (format t
                   "Warning: time spanned by VARI-PULSE is only ~A; TOT-TIME will be reduced accordingly"
                   available-time)
           (ed-beep)
           (setf tot-time available-time))))
  
  ; optimize parameters
  (setf pool (coerce pool 'vector)
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
  (append (unless (zerop offset) 
          (loop until (>= n offset) sum (pop vari-pulse) into n finally return (list (- n))))
        (do-m-mapped-rhythm pool ptable vari-pulse tot-time init-conds
                            (equal pattern-boundary-mode 'NOTE+RESTS))))

;==================================================================================
; DO-M-MAPPED-RHYTHM

(defun do-m-mapped-rhythm (pool ptable vari-pulse tot-time init-conds note+rests?)
  (do ((elapsed-time 0)
       (LDUR nil) (PATTERN-BOUNDARIES nil) (CONTEXT init-conds) ; initialize return values
       (order (table-order ptable)))
      ((>= elapsed-time tot-time)
       (values LDUR PATTERN-BOUNDARIES CONTEXT))
    (let* ((p-row (fetch-row ptable context order elapsed-time))
           (new-pattern-index (get-random-index p-row))
           (new-pattern-list (fetch-pattern pool new-pattern-index elapsed-time))
           (new-pattern-sum (reduce #'+ new-pattern-list :key #'abs))
           (mapped-part-of-pulse (firstn vari-pulse new-pattern-sum))
           (mapped-pattern-dur (apply #'+ mapped-part-of-pulse))
           (mapped-pattern-list (map-pattern new-pattern-list mapped-part-of-pulse))
           (mapped-pattern-boundary-list
            (if note+rests?
              (cons (abs (first mapped-pattern-list))
                    (mapcar #'- (mapcar #'abs (rest mapped-pattern-list))))
              (list mapped-pattern-dur))))
      (incf elapsed-time mapped-pattern-dur)
      (setf vari-pulse (nthcdr new-pattern-sum vari-pulse)
            LDUR (append LDUR mapped-pattern-list)
            PATTERN-BOUNDARIES (append PATTERN-BOUNDARIES mapped-pattern-boundary-list)
            CONTEXT (append (cdr CONTEXT) (list new-pattern-index)))
      )))


(defun map-pattern (pat pulse)
  "maps pat (list of generic durs) onto pulse"
  (let ((multiplier (apply #'lcm (mapcar #'denominator pat))))
    (do-map-pattern (om* pat multiplier) (subdivide-pulse pulse multiplier))))

(defun subdivide-pulse (pulse multiplier)
  (loop for p in pulse
        append (make-list multiplier :initial-element (/ p multiplier))))

(defun do-map-pattern (pat pulse)
  (when pat
    (let* ((generic-dur (first pat))
           (specific-dur (apply #'+ (firstn pulse (abs generic-dur)))))
      (cons (* (signum generic-dur) specific-dur)
            (do-map-pattern (rest pat) (nthcdr (abs generic-dur) pulse))))))

;==================================================================================
; DATA CHECKING

(defun check-pattern-sums (pool)
  "check to ensure each pattern in POOL has an integer sum"
  (when pool
    (if (not (integerp (apply #'+ (mapcar #'abs (first pool)))))
      "M-MAPPED-RHYTHM: each pattern in POOL must have an integer sum"
      (check-pattern-sums (rest pool)))))

(defun check-mapping-params (pool ptable tot-time init-conds)
  "if bad param, returns string describing problem, else returns nil"
  (let ((pool-size (length pool))
        (ptable1 (if (bpf-p (first ptable)) (third ptable) ptable))
        (ptable2 (when (bpf-p (first ptable)) (fourth ptable))))
    (cond ((bpf-p (first pool))
           "M-MAPPED-RHYTHM: bpf-driven interpolation not possible on the pool")
          ((and ptable2 (not (well-formed-ptable? ptable1)))
           "M-MAPPED-RHYTHM: ill-formed first ptable")
          ((and ptable2 (not (well-formed-ptable? ptable2)))
           "M-MAPPED-RHYTHM: ill-formed second ptable")
          ((not (well-formed-ptable? ptable1))
           "M-MAPPED-RHYTHM: ill-formed ptable")
          ((or (and ptable2 (/= pool-size (length ptable2)))
               (/= pool-size (length ptable1)))
           "M-MAPPED-RHYTHM: pool and ptable(s) of incompatible sizes")
          ((not (plusp tot-time))
           "M-MAPPED-RHYTHM: tot-time not a positive number")
          ((and init-conds (>= (apply #'max init-conds) pool-size))
           "M-MAPPED-RHYTHM: init-conds out of range of pool")
          (t (check-pattern-sums pool))           
          )))

;==================================================================================
;==================================================================================
; M-EMPTY-BARS

(defmethod! m-empty-bars (pool ptable tot-time init-conds)
  :icon *tp_icon*
  :doc "
Randomly selects time signatures (representing empty bars) from POOL and
appends them to the output list until the accumulated length is at least
TOT-TIME. Random selections are controlled by the probabilities in PTABLE.

BPF-driven interpolation is possible on the PTABLE as in M-RHYTHM.

The first output is the resulting list of empty bars, and the second
reports the end context, which may serve as initial conditions for the
next in a cascaded series of M-EMPTY-BARS functions."
  :indoc '("list of (numerator denominator) pairs"
           "probability table (flat or nested list) controls random process"
           "target duration of output bars"
           "context for first few random choices")
  :initvals '(nil nil 0 nil)
  :numouts 2
  
  ; tolerate lazy parameter construction
  (when (numberp init-conds) (setf init-conds (list init-conds)))
  
  ; check parameters
  (let ((bad-params (check-bar-params pool ptable tot-time init-conds)))
    (if bad-params (throw-cancel (message-dialog bad-params))))
  
  ; optimize parameters
  (setf pool (coerce pool 'vector)
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
  
  (do-m-empty-bars pool ptable tot-time init-conds))

;==================================================================================
; DO-M-EMPTY-BARS

(defun do-m-empty-bars (pool ptable tot-time init-conds)
  (do ((elapsed-time 0)
       (EMPTY-BARS nil) (CONTEXT init-conds) ; initialize return values
       (order (table-order ptable)))
      ((>= elapsed-time tot-time)
       (values EMPTY-BARS CONTEXT))
    (let* ((p-row (fetch-row ptable context order elapsed-time))
           (new-time-sig-index (get-random-index p-row))
           (new-time-sig (fetch-pattern pool new-time-sig-index elapsed-time)))
      (incf elapsed-time (apply #'/ new-time-sig))
      (setf EMPTY-BARS (append EMPTY-BARS (list new-time-sig))
            CONTEXT (append (cdr CONTEXT) (list new-time-sig-index)))
      )))

;==================================================================================
; DATA CHECKING

(defun check-time-sigs (pool)
  "return nil if good data, message if bad"
  (when pool
    (if (timesigp (first pool))
      (check-time-sigs (rest pool))
      "M-EMPTY-BARS: each time signature in POOL must be a pair of positive integers")))

(defun timesigp (x)
  (and (listp x) (= (length x) 2)
       (integerp (first x)) (plusp (first x))
       (integerp (second x)) (plusp (second x))))

(defun check-bar-params (pool ptable tot-time init-conds)
  (let ((pool-size (length pool))
        (ptable1 (if (bpf-p (first ptable)) (third ptable) ptable))
        (ptable2 (when (bpf-p (first ptable)) (fourth ptable))))
    (cond ((bpf-p (first pool))
           "M-EMPTY-BARS: bpf-driven interpolation not possible on the pool")
          ((and ptable2 (not (well-formed-ptable? ptable1)))
           "M-EMPTY-BARS: ill-formed first ptable")
          ((and ptable2 (not (well-formed-ptable? ptable2)))
           "M-EMPTY-BARS: ill-formed second ptable")
          ((not (well-formed-ptable? ptable1))
           "M-EMPTY-BARS: ill-formed ptable")
          ((or (and ptable2 (/= pool-size (length ptable2)))
               (/= pool-size (length ptable1)))
           "M-EMPTY-BARS: pool and ptable(s) of incompatible sizes")
          ((not (plusp tot-time))
           "M-EMPTY-BARS: tot-time not a positive number")
          ((and init-conds (>= (apply #'max init-conds) pool-size))
           "M-EMPTY-BARS: init-conds out of range of pool")
          (t (check-time-sigs pool))           
          ))
  )

;==================================================================================
;==================================================================================
; M-VARI-PULSE

; a sample well-formed pulse-map is provided at the end of this file

(defmethod! m-vari-pulse (pulse-map ptable empty-bars init-conds)
  :icon *tp_icon*
  :doc "
Makes random selections from PULSE-MAP in order to fill EMPTY-BARS with
a pulse that varies according to the possibilities found in PULSE-MAP.
The result is structured as a list of durations. Random selections are
controlled by the probabilities in PTABLE.

There are several requirements for the input parameters:
- The PULSE-MAP must cover every time signature found in EMPTY-BARS.
  (This is checked by an input-checking routine.)
- For each time signature covered in the PULSE-MAP, at least one pulse
  must have an onset at the beginning of the bar.
  (This is checked by an input-checking routine.)
- Each time a pulse in the PULSE-MAP includes a rest, at least one other
  pulse in PULSE-MAP must include the onset of a note at the same point.
  (This is NOT checked; the user is responsible for handling it.)
- Probabilities in PTABLE may be small but should never be zero.
  (However, the user may specify zero probabilities and these will be
  replaced internally with small non-zero values.)

At each step in the production of a result, a pulse is chosen at random
from the portion of PULSE-MAP matching the current time signature.
Durations are drawn from the chosen pulse until one of two conditions
is met: (1) a rest (as opposed to a note) occurs in the chosen pulse;
(2) a note in the current pulse coincides with a note in at least one
other pulse. Whenever either of these conditions is met, another pulse
is chosen at random, until all the EMPTY-BARS have been filled.

Random choices are controlled by PTABLE, but at each decision point
only pulses that include the onset of a note at the same point may be
chosen.

BPF-driven interpolation is possible on the PTABLE as in M-RHYTHM.

The first output is the resulting list of durations, and the second
reports the end context, which may serve as initial conditions for the
next in a cascaded series of M-VARI-PULSE functions."
  :indoc '("list of sublists, each of form (time-sig pulse-1 pulse-2 ...)"
           "probability table (flat or nested list) controls random process"
           "time signatures representing empty bars"
           "context for first few random choices")
  :numouts 2
  
  ; tolerate lazy parameter construction
  (when (numberp init-conds) (setf init-conds (list init-conds)))
  (setf ptable (zeros->smalls ptable))
  ; pulses for a which a segment doesn't start at a particular position in the bar
  ; will be masked out, so can't risk being left with only zero-probability pulses
  ; therefore, convert zero-probability pulses to small-probability pulses
  
  ; check parameters
  (let ((bad-params (check-pulse-params pulse-map ptable empty-bars init-conds)))
    (if bad-params (throw-cancel (message-dialog bad-params))))
  
  ; optimize parameters
  (setf pulse-map (format-pulse-map pulse-map)
        ptable (if (bpf-p (first ptable))
                 (list (first ptable) (second ptable)
                       (tree->a/r (third ptable))       ; normalize AFTER MASKING (necessarily slower)
                       (tree->a/r (fourth ptable)))
                 (tree->a/r ptable))
        init-conds (let ((init-cond-count (length init-conds)))
                     (if (< init-cond-count (1- *max-ptable-rank*))
                       (append (make-list (- (1- *max-ptable-rank*) init-cond-count)
                                          :initial-element 0)
                               init-conds)
                       (nthcdr (- init-cond-count (1- *max-ptable-rank*))
                               init-conds))))
  
  (do-m-vari-pulse pulse-map ptable empty-bars init-conds))

;==================================================================================
; DO-M-VARI-PULSE

(defun do-m-vari-pulse (pulse-map ptable empty-bars init-conds)
  (do ((elapsed-time 0)
       (LDUR nil) (CONTEXT init-conds) ; initialize return values
       (order (table-order ptable)))
      ((null empty-bars) (values LDUR CONTEXT))
    ; processing a bar's worth in loop
    (loop with bar = (pop empty-bars)
          with pulse-list = (rest (assoc bar pulse-map :test #'equal))
          for p-row = (fetch-row ptable context order elapsed-time)
          for position = 0 then (+ position new-segment-sum)
          until (>= position (apply #'/ bar))
          for masked-p-row = (norm-row (map 'vector #'* (make-p-mask position pulse-list) p-row))       ; normalization done here
          for new-segment-index = (get-random-index masked-p-row)       ; ------------------------------- determines right pulse
          for new-segment-list = (second (assoc position (nth new-segment-index pulse-list)))   ; ------- gets right part of right pulse
          for new-segment-sum = (reduce #'+ new-segment-list :key #'abs)
          do
          (incf elapsed-time new-segment-sum)
          (setf LDUR (append LDUR new-segment-list)
                CONTEXT (append (cdr CONTEXT) (list new-segment-index))))
    ))

;==================================================================================
; ptable masking:

(defun make-p-mask (position pulse-list)
  (loop for pulse in pulse-list
        for seg = (second (assoc position pulse))
        collect (if (and seg (plusp (first seg))) 1 0)))

;==================================================================================
; Parameter formatting

(defun format-pulse-map (pulse-map)
  (loop for time-sig-case in pulse-map
        collect (cons (first time-sig-case)
                      (format-pulse-list (rest time-sig-case)))))

(defun format-pulse-list (pulse-list)
  (loop for n from 0 to (1- (length pulse-list))
        for pulse = (nth n pulse-list)
        and other-pulses = (append (firstn pulse-list n)
                                   (nthcdr (1+ n) pulse-list))
        for other-points =
        (sort (remove-duplicates (loop for o-p in other-pulses
                                       append (dx->x 0 (mapcar #'abs o-p))))
              #'<)
        collect (segment-pulse pulse other-points)))

(defun segment-pulse (pulse points &optional (position 0) segment)
  "
segment PULSE so a new group begins at each coincidence with POINTS;
format each segment as a pair (position ldur), where ldur is a list
of durations (extracted from PULSE) and position locates the onset
of the first of these durations relative to the beginning of the
current bar"
  (when pulse
    (let ((new-position (+ position (abs (first pulse)))))
      (if (member new-position points)
        (cons (list (first (last (or segment (list position))))
                    (reverse (cons (first pulse) (butlast segment))))
              (segment-pulse (rest pulse) points new-position))
        (segment-pulse (rest pulse) points new-position (cons (first pulse)
                                                              (or segment
                                                                  (list position))))))))

(defun zeros->smalls (ptable)
  (when ptable
    (cond ((bpf-p (first ptable)) (list (first ptable) (second ptable)
                                        (zeros->smalls (third ptable))
                                        (zeros->smalls (fourth ptable))))
          ((numberp (first ptable))
           (let ((small (* .001 (loop for p in ptable unless (zerop p) minimize p))))
             (loop for p in ptable collect (if (zerop p) small p))))
          (t (cons (zeros->smalls (first ptable))
                   (zeros->smalls (rest ptable)))))))

;==================================================================================
; DATA CHECKING

(defun check-pulse-params (pulse-map ptable empty-bars init-conds)
  (let ((pool-size (1- (length (first pulse-map))))
        (ptable1 (if (bpf-p (first ptable)) (third ptable) ptable))
        (ptable2 (when (bpf-p (first ptable)) (fourth ptable))))
    
    (cond
     ((not (apply #'= (mapcar #'length pulse-map)))
      "M-VARI-PULSE: number of pulses in PULSE-MAP is inconsistent for different time signatures")
     ((missing-downbeat pulse-map)
      "M-VARI-PULSE: for one or more time signatures in PULSE-MAP, all of the pulses begin with rests")
     ((dead-end-rest pulse-map)
      "M-VARI-PULSE: a rest in one pulse doesn't coincide with the onset of a note in at least one other pulse")
     ((wrong-size-pulse pulse-map)
      "M-VARI-PULSE: for one or more time signatures in PULSE-MAP, a pulse is the wrong size to fill the bar")
     ((missing-time-sig pulse-map empty-bars)
      "M-VARI-PULSE: time signature in EMPTY-BARS not found in PULSE-MAP")
     ((and ptable2 (not (well-formed-ptable? ptable1)))
      "M-VARI-PULSE: ill-formed first ptable")
     ((and ptable2 (not (well-formed-ptable? ptable2)))
      "M-VARI-PULSE: ill-formed second ptable")
     ((not (well-formed-ptable? ptable1))
      "M-VARI-PULSE: ill-formed ptable")
     ((or (and ptable2 (/= pool-size (length ptable2)))
          (/= pool-size (length ptable1)))
      "M-VARI-PULSE: size of ptable(s) incompatible with number of pulses in PULSE-MAP")
     ((and init-conds (>= (apply #'max init-conds) pool-size))
      "M-EMPTY-BARS: INIT-CONDS out of range of PULSE-MAP")
     ))
  )

;----- auxiliaries for data checking in M-VARI-PULSE

(defun missing-downbeat (pulse-map)
  "T if all pulses begin with rests in at least one sublist, NIL otherwise"
  (when pulse-map
    (or (minusp (apply #'max (mapcar #'first (rest (first pulse-map)))))
        (missing-downbeat (rest pulse-map)))))

(defun dead-end-rest (pulse-map)
  "does any pulse include a rest NOT coinciding with the onset of a note in another pulse for the same time signature"
  (when pulse-map
    (let* ((pulse-group
            (rest (first pulse-map)))
           (onsets ; code rests as negatives
            (remove-duplicates (loop for pulse in pulse-group
                                     for onsets = (rest (dx->x 0 (mapcar #'abs pulse)))
                                     for signs = (mapcar #'signum (rest pulse))
                                     append (mapcar #'* onsets signs)))))
      (or (loop for o in onsets
                when (and (minusp o)
                          (not (member (- o) onsets)))
                return t)
          (dead-end-rest (rest pulse-map))))))

(defun wrong-size-pulse (pulse-map)
  (when pulse-map
    (let ((time-sig (first (first pulse-map)))
          (pulse-group (rest (first pulse-map))))
      (or (not (apply #'= (cons (apply #'/ time-sig)
                                (mapcar (lambda (pulse)
                                          (apply #'+ (mapcar #'abs pulse)))
                                        pulse-group))))
          (wrong-size-pulse (rest pulse-map))))))

(defun missing-time-sig (pulse-map empty-bars)
  (let ((pulse-map-time-sigs (mapcar #'first pulse-map))
        (empty-bars-time-sigs (remove-duplicates empty-bars :test 'equal)))
    (loop for time-sig in empty-bars-time-sigs
          unless (member time-sig pulse-map-time-sigs :test 'equal)
          return t)))

;=================================================================
;=================================================================
;=================================================================

#|
;sample of a well-formed PULSE-MAP parameter:

(((2 4)
  (1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16)
  (1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20)
  (-2/4)
  (1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24))
 ((5 8)
  (1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16)
  (1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 -1/8)
  (-1/8 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20)
  (1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24))
 ((3 4)
  (1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16 1/16)
  (1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20 1/20)
  (-3/4)
  (1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24 1/24)))
|#
