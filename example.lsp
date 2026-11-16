;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This file provides examples for using cl-waveset
;; Feel free to play around or look at the source code to see how things
;; work!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload :cl-waveset)
(in-package :cl-waveset)

(let* ((filespec "/path/to/file.wav")
       ;;; First read an audiofile:
       ;;; read-audio-file returns a file as a list of:
       ;;; 1. A List of vectors. Every vector in the list contains the samples of one channel
       ;;; 2. the samplerate
       ;;; We load the left channel of a stereo file like this:
       (sample-array (first (first (read-audio-file-samples filespec))))

       ;;; We analyse our samples as an array of wavesets
       (wavesets (analize-wavesets sample-array))

       ;;; we then perform any number manipulations on our waveset-array,
       ;;; i.e. shuffling the wavesets
       (wavesets-shuffled (shuffle-wavesets (copy-seq wavesets) 0.4))

       ;;; we then render a new sample array based on our shuffled waveset-array
       (samples-shuffled (make-array-from-wavesets wavesets-shuffled data-array3))

       ;;; These are examples of other wavesetmanipulations

       ;; repeating -> repeats Wavesets n times (a sort of crude time stretching)
       ;; (repeated-wavesets (repeat-all-wavesets-n-times wavesets 20))
       ;; (repeated-array (make-array-from-wavesets repeated-wavesets data-array3))

       ;;; combination -> interleaves the wavesets of multiple sounds to blend them together
       ;; (combined-array (combine-wavesets (list wavesets1 wavesets2 wavesets3)
       ;; 					 (list sample-array1 sample-array2 sample-array3)
       ;; 					 :zerocr-smoothing t)))

       ;;; omission -> ommits wavesets by chance.
       ;;              You can also change the chance over the duration of the file
       ;; (thinned-wavesets (thin-wavesets-by-chance-curve wavesets 0
       ;;                                                  (1- (length wavesets)) 0 0.3))
       ;; (thin-array (make-array-from-wavesets-at-place data-array thinned-wavesets)))

       ;;; substitution -> substitutes wavesets by saw or square waves of equal duration
       ;;               Can also be changed over the duration of a file
       ;; (saw-array (sort-wavesets-by-chance-curve wavesets sample-array 0 (/ (length wavesets) 2)
       ;; 						 0 1))
       ;; (square-array (square-wavesets-by-chance-curve wavesets sample-array 0
       ;; 						      (/ (length wavesets) 2)
       ;; 						      0 1))

       ;;; pitching -> pitch wavesets according to a specified curve
       ;; (pitched-arr (pitch-wavesets-by-parameter wavesets sample-array 0 1 1 1 0.5 0.3
       ;; 						 :index-fun #'(lambda (i) (= 0 (mod i 3)))))

       ;;; equalization -> equalizes the duration of all wavesets in a sound,
       ;;                  forcing a pitch specified in the function
       ;;        (equalized-arr (equalize-waveset-lengths wavesets sample-array 50 :zerocr-smoothing nil))

       ;;; shaking -> mixes the duration ratios between adjacent pairs of wavesets
       ;; (shaken-arr (shake-wavesets wavesets sample-array 0.5))

       ;;; harmonizing -> adds harmonic Multiples of Wavesets to themselves
       ;; (harm-arr (harmonize-wavesets wavesets sample-array 4))

       ;;; mixing -> stretches the waveset durations of one sound to the durations of another
       ;;            creating a mix of both.
       ;; (mixed-arr (stretch-to-different-wavesets wavesets1 sample-array-1 wavesets-2
       ;; 						 :zerocr-smoothing t))

       ;;; spatialisation -> you can distribute wavesets consecutively to new  channels
       ;;                    by a defined sequence -> creates new list of vectors
       ;;                    each corresponding to a channel
       ;; (spatialised-sample-array-list (circle-wavesets-spatially wavesets sample-array '(0 1 2 3 4 5 6 7) 8))

       ;;; a lot of them can be mixed or applied consecutively


       ;;; finally write a processed sample array
  (write-audio-file-samples "/path/to/output.wav" (list sample-array) 44100))
