;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; So 9. Jul 20:47:07 CEST 2023
;;;
;;; Main-file: loads all necessary components of the Programm
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage #:cl-waveset
  (:use #:common-lisp #:cl-libsndfile)
  (:export #:read-audio-file-samples
           #:write-audio-file-samples
           #:analize-wavesets
           #:shuffle-wavesets
           #:repeat-all-wavesets-n-times
           #:combine-wavesets
           #:thin-wavesets
           #:spread-wavesets-spatially
           #:circle-wavesets-spatially
           #:thin-wavesets-by-chance-curve
           #:make-array-from-wavesets-at-place
           #:circle-wavesets-spatially
           #:sort-wavesets-by-chance-curve
           #:square-wavesets-by-chance-curve
           #:pitch-wavesets-by-parameter
           #:equalize-waveset-lengths
           #:shake-wavesets
           #:harmonize-wavesets
           #:stretch-to-different-wavesets
           #:make-array-from-wavesets))
