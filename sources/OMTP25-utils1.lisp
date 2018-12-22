;;;;; TimePack version 2.0, Spring 2004
;;;;; an OpenMusic library for creating and manipulating rhythms

;;;;; version 2.5 (no changes to this file)

;;;;; conceived and programmed by Paul Nauert,
;;;;; University of California, Santa Cruz

; in this file
; CONFORM
; TIMEPOINTS->DENSITY

;==================================================================================
;==================================================================================
; TIMEPACK UTILITIES 1 of 2
;==================================================================================
; TIMEPOINTS->DENSITY

(defun do-timepoints->density (timepoints framesize tmin tmax)
  (loop for frame-start upfrom tmin by framesize
        and counter = 0
        while (and timepoints (<= frame-start tmax))
        do (loop for tp in timepoints
                 while (< tp (+ frame-start framesize))
                 do (pop timepoints) (incf counter))
        collect frame-start into x-points
        collect counter into y-points
        finally (return (values x-points y-points))))

;==================================================================================
; CONFORM

(defun do-conform (in-points to-points &optional lim)
  (unless (null in-points)
    (let* ((pt (car in-points))
           (best (best-approx pt to-points))
           (err (abs (- best pt))))
      (if (and lim (> err (abs lim)))
        (if (< lim 0)
          (do-conform (cdr in-points) to-points lim)
          (cons pt
                (do-conform (cdr in-points) to-points lim)))
        (cons best
              (do-conform (cdr in-points) to-points lim))))))


(defun best-approx (x lst)
  "Returns the member of LST that best approximates X."
  (do* ((srch-lst lst (cdr srch-lst))
        (elt (car srch-lst) (car srch-lst))
        (best elt
              (if (< (abs (- elt x)) (abs (- best x)))
                elt best)))
       ;termination:
       ((null (cdr srch-lst)) best)))


;==================================================================================
;==================================================================================
; ** OM Methods **

(defmethod! TIMEPOINTS->DENSITY (timepoints framesize tmin tmax) 
  :icon *tp_util_icon*
  :doc "
Counts the number of TIMEPOINTS in successive frames of size FRAMESIZE, starting at
TMIN and continuing until TMAX is reached or exceeded. Returns two lists: X-POINTS
lists the startpoints of successive frames, and Y-POINTS counts the timepoints in
each frame. These lists can be connected to the bpf-factory inlets of the same names
to create a bpf that plots the fluctuating density of TIMEPOINTS per frame."
  :initvals '(nil 10 0 100)
  :indoc '("list of timepoints"
           "unit for density measurement"
           "first frame starts"
           "last frame ends at or beyond")
  :numouts 2
  (do-timepoints->density timepoints framesize tmin tmax))

;==================================================================================

(defmethod! CONFORM (in-points to-points &optional lim)
  :icon *tp_util_icon*
  :doc "
Replaces each point P in IN-POINTS with best approx Q from TO-POINTS.
If difference between P and Q exceeds the absolute value |LIM|, sends
P out unchanged [nonnegative LIM] or deletes P entirely [negative LIM]."
  :indoc '("list of timepoints"
           "list of timepoints"
           "limits amount that elements of IN-POINTS can change")
  (do-conform in-points to-points &optional lim))
