;;;;; TimePack version 2.0, Spring 2004
;;;;; an OpenMusic library for creating and manipulating rhythms

;;;;; version 2.5 (no changes to this file)

;;;;; conceived and programmed by Paul Nauert,
;;;;; University of California, Santa Cruz

; in this file
; RESIZE-PTABLE
; GM-PGMOUT
; THRU

(in-package :om)

;==================================================================================
; TIMEPACK UTILITIES 2 of 2

(defun resize-row (row resize-pairs)
  "
Tries to express the shape of ROW while re-sizing it according to RESIZE-PAIRS. As
an element of RESIZE-PAIRS, (X Y) is interpreted as an instruction to approximate
the shape of the next X elements of ROW in the next Y elements of the result.
Smoothing occurs as a side effect, which can be pronounced when the difference
between the old sizes (like X) and the new sizes (like Y) is large."

; check parameters
  (let ((old-sizes (mapcar #'first resize-pairs))
        (new-sizes (mapcar #'second resize-pairs)))
    (and row
         (plusp (apply #'* old-sizes))          ; no zero old sizes
         (= (apply #'+ old-sizes) (length row))
         (do-resize-row row old-sizes new-sizes))))


(defun do-resize-row (row old-sizes new-sizes)
  (when row
    (append (resize-row-by-n (firstn row (first old-sizes))
                         (- (first new-sizes) (first old-sizes)))
            (do-resize-row (nthcdr (first old-sizes) row)
                           (rest old-sizes) (rest new-sizes)))))


(defun resize-row-by-n (row n)
  (cond ((zerop n) row)
        ((plusp n) (resize-row-by-n (one-more row) (1- n)))
        ((minusp n) (resize-row-by-n (one-less row) (1+ n)))))


(defun one-less (row)
  (when (rest row)
    (cons (/ (+ (first row) (second row)) 2)
          (one-less (rest row)))))


(defun one-more (row)
  (let* ((old-sum (apply #'+ row))
         (pre-normed (cons (first row)
                           (append (one-less row)
                                   (last row))))
         (new-sum (apply #'+ pre-normed)))
    (if (zerop new-sum)
      pre-normed
      (mapcar (lambda (x) (* x old-sum (/ new-sum)))
            pre-normed))))


(defun do-resize-table (table old-sizes new-sizes)
  (when table
    (append (resize-table-by-n (firstn table (first old-sizes))
                               (- (first new-sizes) (first old-sizes)))
            (do-resize-table (nthcdr (first old-sizes) table)
                             (rest old-sizes) (rest new-sizes)))))


(defun resize-table-by-n (table n)
  (cond ((zerop n) table)
        ((plusp n) (resize-table-by-n (one-more-row table) (1- n)))
        ((minusp n) (resize-table-by-n (one-less-row table) (1+ n)))))


(defun one-less-row (table)
  (when (rest table)
    (cons (mapcar (lambda (1st 2nd) (/ (+ 1st 2nd) 2))
                  (first table) (second table))
          (one-less-row (rest table)))))


(defun one-more-row (table)
  (cons (first table)
        (append (one-less-row table)
                (last table))))

;===========================================================================
;===========================================================================
; ** OM Method **

(defmethod! RESIZE-PTABLE (ptable resize-pairs)
  :icon *resize_icon*
  :doc "
Tries to express the shape of 1- or 2-dim PTABLE in a new probability table
related to the original in a way expressed by RESIZE-PAIRS. The first pair
(X Y) means 'express the first X elements (or rows and columns) of PTABLE in
  the first Y elements (rows/columns) of the result', and successive pairs
control the processing of successive elements (rows/columns). For example,
the RESIZE-PAIRS list ((7 8)) takes a length-7 or 7x7 PTABLE and returns a
length-8 or 8x8 result; the RESIZE-PAIRS list ((4 4) (3 4)) does likewise but
concentrates the change in the last three elements (rows/columns).


DETAILS: How 1-dim tables and the rows of 2-dim tables are processed

When the number of elements INCREASES, the new result
is scaled to have the same sum as the original:

    (resize-ptable '(1) '((1 2))) ==> (1/2 1/2)
    (resize-ptable '(3 6) '((2 3))) ==> (2 3 4)
    (resize-ptable '(28 70 42) '((3 6))) ==> (16 19 26 29 26 24)

When the number of elements DECREASES, there are three cases.
[1] a decrease to length zero yields NIL; [2] a decrease from
length 2 to length 1 yields the average of the original two
elements; [3] a decrease from length N to length (N - 1) is
scaled to have the sum (A + B), where A is the average of the
first and last of the original elements, and B is the sum of
the remaining original elements.

    (resize-ptable '(5) '((1 0))) ==> NIL ; [1]
    (resize-ptable '(1 3) '((2 1))) ==> (2) ; [2]
    (resize-ptable '(2 12 6) '((3 2))) ==> (7 9) ; [3]"
  :initvals '((1) ((1 2)))
  :indoc '("1- or 2-dim table (flat or nested list)"
           "list of pairs to control resizing")
  
  (case (leftmost-depth ptable)
    (1 (resize-row ptable resize-pairs))
    (2 (do-resize-table (mapcar (lambda (row) (resize-row row resize-pairs))
                                ptable)
                        (mapcar #'first resize-pairs)
                        (mapcar #'second resize-pairs)))
    (t (throw-cancel
        (om-message-dialog 
         "RESIZE-PTABLE only works with one- and two-dimensional tables.")))))

;===========================================================================
;===========================================================================
; OTHER UTILITIES

;============================================================================
; a little MIDI extra

(defmethod! GM-PGMOUT (pgm chan)
  :icon '(148)
  :doc "sends program-change message based on General MIDI patch name"
  :menuins '((0 (("1 Acoustic Grand Piano" 1) ("2 Bright Acoustic Piano" 2) ("3 Electric Grand Piano" 3)
                ("4 Honky-tonk Piano" 4) ("5 Rhodes Piano" 5) ("6 Chorused Piano" 6) ("7 Harpsichord" 7) ("8 Clavinet" 8)
                ("9 Celesta" 9) ("10 Glockenspiel" 10) ("11 Music Box" 11) ("12 Vibraphone" 12) ("13 Marimba" 13)
                ("14 Xylophone" 14) ("15 Tubular Bells" 15) ("16 Dulcimer" 16) ("17 Hammond Organ" 17)
                ("18 Percussive Organ" 18) ("19 Rock Organ" 19) ("20 Church Organ" 20) ("21 Reed Organ" 21)
                ("22 Accordion" 22) ("23 Harmonica" 23) ("24 Tango Accordion" 24) ("25 Acoustic Guitar (nylon)" 25)
                ("26 Acoustic Guitar (steel)" 26) ("27 Electric Guitar (jazz)" 27) ("28 Electric Guitar (clean)" 28)
                ("29 Electric Guitar (muted)" 29) ("30 Overdriven Guitar" 30) ("31 Distortion Guitar" 31)
                ("32 Guitar Harmonics" 32) ("33 Acoustic Bass" 33) ("34 Electric Bass (finger)" 34)
                ("35 Electric Bass (pick)" 35) ("36 Fretless Bass" 36) ("37 Slap Bass 1" 37) ("38 Slap Bass 2" 38)
                ("39 Synth Bass 1" 39) ("40 Synth Bass 2" 40) ("41 Violin" 41) ("42 Viola" 42) ("43 Cello" 43)
                ("44 Contrabass" 44) ("45 Tremolo Strings" 45) ("46 Pizzicato Strings" 46) ("47 Orchestral Harp" 47)
                ("48 Timpani" 48) ("49 String Ensemble 1" 49) ("50 String Ensemble 2" 50) ("51 SynthStrings 1" 51)
                ("52 SynthStrings 2" 52) ("53 Choir Aahs" 53) ("54 Voice Oohs" 54) ("55 Synth Voice" 55)
                ("56 Orchestra Hit" 56) ("57 Trumpet" 57) ("58 Trombone" 58) ("59 Tuba" 59) ("60 Muted Trumpet" 60)
                ("61 French Horn" 61) ("62 Brass Section" 62) ("63 Synth Brass 1" 63) ("64 Synth Brass 2" 64)
                ("65 Soprano Sax" 65) ("66 Alto Sax" 66) ("67 Tenor Sax" 67) ("68 Baritone Sax" 68) ("69 Oboe" 69)
                ("70 English Horn" 70) ("71 Bassoon" 71) ("72 Clarinet" 72) ("73 Piccolo" 73) ("74 Flute" 74)
                ("75 Recorder" 75) ("76 Pan Flute" 76) ("77 Bottle Blow" 77) ("78 Shakuhachi" 78) ("79 Whistle" 79)
                ("80 Ocarina" 80) ("81 Lead 1 (square)" 81) ("82 Lead 2 (sawtooth)" 82) ("83 Lead 3 (calliope lead)" 83)
                ("84 Lead 4 (chiff lead)" 84) ("85 Lead 5 (charang)" 85) ("86 Lead 6 (voice)" 86)
                ("87 Lead 7 (fifths)" 87) ("88 Lead 8 (bass + lead)" 88) ("89 Pad 1 (new age)" 89) ("90 Pad 2 (warm)" 90)
                ("91 Pad 3 (polysynth)" 91) ("92 Pad 4 (choir)" 92) ("93 Pad 5 (bowed)" 93) ("94 Pad 6 (metallic)" 94)
                ("95 Pad 7 (halo)" 95) ("96 Pad 8 (sweep)" 96) ("97 FX 1 (rain)" 97) ("98 FX 2 (soundtrack)" 98)
                ("99 FX 3 (crystal)" 99) ("100 FX 4 (atmosphere)" 100) ("101 FX 5 (brightness)" 101)
                ("102 FX 6 (goblins)" 102) ("103 FX 7 (echoes)" 103) ("104 FX 8 (sci-fi)" 104) ("105 Sitar" 105)
                ("106 Banjo" 106) ("107 Shamisen" 107) ("108 Koto" 108) ("109 Kalimba" 109) ("110 Bagpipe" 110)
                ("111 Fiddle" 111) ("112 Shanai" 112) ("113 Tinkle Bell" 113) ("114 Agogo" 114) ("115 Steel Drums" 115)
                ("116 Woodblock" 116) ("117 Taiko Drum" 117) ("118 Melodic Tom" 118) ("119 Synth Drum" 119)
                ("120 Reverse Cymbal" 120) ("121 Guitar Fret Noise" 121) ("122 Breath Noise" 122) ("123 Seashore" 123)
                ("124 Bird Tweet" 124) ("125 Telephone Ring" 125) ("126 Helicopter" 126) ("127 Applause" 127)
                ("128 Gunshot" 128))))
  :indoc '("patch name" "MIDI channel")
  :initvals '(1 1)
  (pgmout pgm chan))

;============================================================================
; and an onscreen-layout utility

(defmethod! THRU (x)
  :icon *thru_icon*
  :doc "sends input through to output"
  x)
