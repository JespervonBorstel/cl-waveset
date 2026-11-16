;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; So 9. Jul 20:50:58 CEST 2023
;;;
;;; Contains all functions for analizing audio-arrays and wavesets
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-waveset)

;;;
;; Analizing an Audio-array to get the Indices of Zerocrossings
;;;

(defun analize-wavesets-1 (samp-arr)
  (let* ((zerocr-arr (make-array '(0) :fill-pointer 0)))
    (loop for i from 1 below (length samp-arr)
	  if (> 0 (* (elt samp-arr i) (elt samp-arr (1- i))))
	    do (vector-push-extend (1- i) zerocr-arr))
    (concatenate 'vector #(0) zerocr-arr)))

(defun analize-wavesets-2 (zerocr-arr)
  "sammelt jeden zweiten nulldurchgang"
  (let* ((end-arr (make-array 0 :fill-pointer 0)))
    (loop for i from 0 below (length zerocr-arr) by 2
	  do (vector-push-extend (elt zerocr-arr i) end-arr))
    end-arr))
	
(defun analize-wavesets-3 (waveset-arr samp-arr)
  "falls der letzte Wert fehlt, weil es keine ungerade Anzahl von Nulldurchgängen gibt,
   wird er hinten angebracht"
  (let* ((l (length samp-arr))
	 (last-i (elt waveset-arr (1- (length waveset-arr)))))
    (if (= last-i (1- l))
	waveset-arr
	(progn (vector-push (1- l) waveset-arr)
	       waveset-arr))))

(defun analize-wavesets-4 (waveset-arr)
  "listet die indizes eines Wavesets"
  (let* ((end-arr (make-array 0 :fill-pointer 0 :element-type 'list)))
    (loop for i from 1 below (length waveset-arr)
	  do (vector-push-extend (list (elt waveset-arr (1- i)) (1- (elt waveset-arr i)))
				 end-arr))
    end-arr))

(defun analize-wavesets (samp-arr)
  "führt alle vorigen schritte zusammen"
  (analize-wavesets-4
   (analize-wavesets-3 (analize-wavesets-2 (analize-wavesets-1 samp-arr)) samp-arr)))

;;;
;; Functions analizing wavesets/arrays of samples
;;;

(defun get-waveset-length (waveset &optional samps)
  "gibt die anzahl an samples in einem Waveset zurück"
  (declare (ignore samps))
  (1+ (- (cadr waveset) (car waveset))))

(defun get-array-length (wavesets)
  "returns how many samples a set of wavesets is long"
  (loop for i below (length wavesets)
	sum (get-waveset-length (elt wavesets i))))

(defun get-waveset-loudness (waveset samp-arr)
  "returns the rms-value of one waveset"
  (/ (loop for i from (car waveset) to (cadr waveset)
	   sum (sqrt (expt (elt samp-arr i) 2)))
     (- (cadr waveset) (car waveset))))
	   
(defun get-waveset-loudness-arr (wavesets samp-arr)
  "returns the rms-values of all wavesets in a waveset-array"
  (let* ((end-arr (make-array (length wavesets) :fill-pointer 0 )))
    (loop for i below (length wavesets)
          do (vector-push (get-waveset-loudness (elt wavesets i) samp-arr) end-arr))
    end-arr))

(defun get-loudest-sample (samp-arr)
  (loop for i below (length samp-arr)
	maximize (abs (elt samp-arr i))))

;;;
;; Functions giving access to the samples of wavesets
;;;

(defun get-waveset (waveset samp-arr)
  "gibt ein einzelnes Waveset als array der sample-werte aus"
  (let* ((start-i (car waveset))
	 (end-i (cadr waveset))
	 (arr (make-array (1+ (- end-i start-i)) :fill-pointer 0)))
    (loop for i from start-i to end-i
	  do (vector-push (elt samp-arr i) arr))
    arr))

