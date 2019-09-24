;;;;; TimePack version 2.0, Spring 2004
;;;;; an OpenMusic library for creating and manipulating rhythms

;;;;; version 2.5 (only changing a function name: mpartition in 2.0 = m-partition in 2.5)

;;;;; conceived and programmed by Paul Nauert,
;;;;; University of California, Santa Cruz

; in this file
; M-PARTITION

#|
ABOUT OPTIMIZED PARAMETERS
For the sake of speed, probability tables and pools are converted internally
from lists to special array structures: 1-dim tables and pools are converted
to simple vectors, and larger-dim tables are converted to arrays of simple
vectors. This last structure is identified in certain function and variable
names as 'a/r' (for 'array of rows').
|#

(in-package :om)

; cf interpolation in...
; ...mrhythm (bpf start min max)
; ...M-PARTITION (bpf min max) --optimize--> (bpf 0 min max)
;===================================================================================
;===================================================================================
; M-PARTITION

(defmethod! M-PARTITION (timepoints ptable init-conds)
  :icon *tp_icon*
  :doc "
Rhythm represented by TIMEPOINTS is distributed across sublists of first output,
according to Markov process controlled by PTABLE. When order of Markov process
exceeds zero, INIT-CONDS provides context for first few random selections. Number
of sublists is implicit in the shape/size of PTABLE. Second output reports context
at end of process, which allows MARKPARTITION modules to be cascaded, with each
stage inheriting appropriate initial conditions."
  :indoc '("list of timepoints to partition"
           "probability table (flat or nested list) to control random partitioning"
           "context for first few random choices")
  :initvals '(nil '(1 1) nil)
  :numouts 2
  
  ; tolerate lazy parameter construction
  (when (numberp init-conds) (setf init-conds (list init-conds)))
  
  ; check parameters
  (let ((bad-params
         (check-mpartition-params timepoints ptable init-conds)))
    (if bad-params (throw-cancel (message-dialog bad-params))))
  
  ; optimize parameters
  (setf ptable (if (bpf-p (first ptable))
                 (list (first ptable) 0
                       (normalize-rows (tree->a/r (second ptable)))
                       (normalize-rows (tree->a/r (third ptable))))
                 (normalize-rows (tree->a/r ptable)))
        init-conds (let ((init-cond-count (length init-conds)))
                     (if (< init-cond-count (1- *max-ptable-rank*))
                       (append (make-list (- (1- *max-ptable-rank*) init-cond-count)
                                          :initial-element 0)
                               init-conds)
                       (nth-cdr (- init-cond-count (1- *max-ptable-rank*))
                                init-conds))))
  
  (do-mpartition timepoints ptable init-conds))


;===================================================================================
; DO-MPARTITION

(defun do-mpartition (timepoints ptable init-conds)
  (do* ((size (array-dimension
               (if (listp ptable) (third ptable) ptable) 0))
        (RESULT (make-list size)) (CONTEXT init-conds) ; initialize return values
        (order (table-order ptable))
        (remaining timepoints (rest remaining)))
       ((null remaining) (values RESULT CONTEXT))
    (let* ((tp (first remaining))
           (p-row (fetch-row ptable context order tp))
           (index (get-random-index p-row)))
      (setf (nth index RESULT) (append (nth index RESULT) (list tp))
            CONTEXT (append (cdr CONTEXT) (list index))))))

;===================================================================================
;===================================================================================
; AUX
;===================================================================================
; - INPUT CHECKING
;   most of the work is done by functions in the MRHYTHM code

(defun check-mpartition-params (timepoints ptable init-conds)
  (let* ((ptable1 (if (bpf-p (first ptable)) (second ptable) ptable))
         (ptable2 (when (bpf-p (first ptable)) (third ptable)))
         (size (length ptable1)))
    (cond ((and ptable2 (not (well-formed-ptable? ptable1)))
           "M-PARTITION: ill-formed first ptable")
          ((and ptable2 (not (well-formed-ptable? ptable2)))
           "M-PARTITION: ill-formed second ptable")
          ((not (well-formed-ptable? ptable1))
           "M-PARTITION: ill-formed ptable")
          ((and ptable2 (/= size (length ptable2)))
           "M-PARTITION: first and second ptables of incompatible sizes")
          ((and init-conds (>= (apply #'max init-conds) size))
           "M-PARTITION: init-conds out of range")
          )))

