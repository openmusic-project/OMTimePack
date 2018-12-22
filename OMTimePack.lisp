;;;;============================================================================
;;;; OMTP_2.5.lib
;;;;
;;;; OMTimePack library version 2.5
;;;; author: Paul Nauert
;;;;
;;;; VERSION 1, 14 October 2001
;;;; with the assistance of C. Agon, G. Assayag, K. Haddad
;;;; and the support of the University of California, Santa Cruz Faculty Senate
;;;; and the support of IRCAM, Scientific Division
;;;;
;;;; VERSION 2, 15 April 2004
;;;; with the support of the University of California, Santa Cruz Faculty Senate
;;;; and the Columbia University Department of Music
;;;;
;;;; VERSION 2.5, 25 August 2005
;;;;
;;;;============================================================================ 

(in-package :om)

;--------------------------------------------------
;Variable definition with files to load 
;--------------------------------------------------
 
(defvar *sources-dir* "source files")
(setf *sources-dir* (append (pathname-directory *load-pathname*) (list "sources")))

(defvar *TimePack-lib-files* nil)
(setf *TimePack-lib-files*
      '("OMTP25-m-rhythm" 
        "OMTP25-m-partn"
        "OMTP25-mapping"
        "OMTP25-utils1"
        "OMTP25-utils2"))

;--------------------------------------------------
;Loading & compiling files 
;--------------------------------------------------

(mapc #'(lambda (file) (compile&load (make-pathname :directory *sources-dir* :name file))) *TimePack-lib-files*)

;--------------------------------------------------
; OM subpackages initialization
; ("sub-pack-name" subpacke-lists class-list function-list class-alias-list)
;--------------------------------------------------

(defvar *subpackages-list* nil)
(setf *subpackages-list*
      '(("Generators" nil nil (M-RHYTHM M-MAPPED-RHYTHM M-EMPTY-BARS M-VARI-PULSE) nil)
        ("Transformations" nil nil (CONFORM M-PARTITION) nil)
        ("Utilities" nil nil (TIMEPOINTS->DENSITY RESIZE-PTABLE THRU GM-PGMOUT) nil)))

;--------------------------------------------------
;filling packages
;--------------------------------------------------

(om::fill-library *subpackages-list*)
