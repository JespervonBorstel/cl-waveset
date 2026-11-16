;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; So 9. Jul 21:18:15 CEST 2023
;;;
;;; Utilities for Handling Audio like pitch-shifting, filters etc.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-waveset)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Zerocrossing-smoothing
;; needs the array to have its zerocrossings already analized
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-zerocr-poli (samp1 samp2 samp3 samp4)
  "returns the interpolation-polinomial that smoothes the zerocrossing"
  (let* ((a (/ (+ (* -3 samp1) (* 5 samp2) (* -5 samp3) (* 3 samp4)) 60))
	 (b (/ (+ (* 2 samp1) (* -3 samp2) (* 2 samp3) (* -1 samp4)) 4))
	 (c (/ (+ (* -87 samp1) (* 100 samp2) (* -25 samp3) (* 12 samp4)) 60))
	 (d samp1))
    (lambda (x)
      (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d))))

(defun smooth-zerocrossings (wavesets samp-arr)
  (let* ((end-arr (make-array (length samp-arr) :fill-pointer 0)))
    (loop for i below (- (length wavesets) 2)
	  do (let* ((waveset1 (elt wavesets i))
		    (waveset2 (elt wavesets (1+ i)))
		    (last-waveset1 (cadr waveset1))
		    (first-waveset2 (car waveset2))
		    (samp1 (elt samp-arr (- last-waveset1 3)))
		    (samp2 (elt samp-arr (- last-waveset1 2)))
		    (samp3 (elt samp-arr first-waveset2))
		    (samp4 (elt samp-arr (+ first-waveset2 1)))
		    (waveset1-arr (get-waveset waveset1 samp-arr))
		    (zerocr-poli (make-zerocr-poli samp1 samp2 samp3 samp4)))
	       (setf (elt waveset1-arr
			  (- (1- last-waveset1) (car waveset1)))
		     (funcall zerocr-poli 2)
		     (elt waveset1-arr
			  (- last-waveset1 (car waveset1)))
		     (funcall zerocr-poli 3))
	       (loop for j below (length waveset1-arr)
		     do (vector-push (elt waveset1-arr j) end-arr))))
    (concatenate 'vector
		 end-arr
		 (get-waveset (elt wavesets (1- (length wavesets))) samp-arr))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tape-Timestretching
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-poli (samp1 samp2 samp3 samp4)
  "macht ein interpolationspolinom 3. Ordnung"
  (let* ((a (/ (+ (* -1 samp1) (* 3 samp2) (* -3 samp3) samp4) 6))
	 (b (/ (+ (* 2 samp1) (* -5 samp2) (* 4 samp3) (* -1 samp4)) 2))
	 (c (/ (+ (* -11 samp1) (* 18 samp2) (* -9 samp3) (* 2 samp4)) 6))
	 (d samp1))
    (lambda (x)
      (+ (* a (expt x 3)) (* b (expt x 2)) (* c x) d))))

(defun stretch-array (samps factor)
  "timestretching mit 4-Punkt interpolation"
  (when (>= 0 factor)
    (error "Factor can't be 0 or smaller!"))
  (let* ((l (length samps)))
    (if (> l 4)
	(let* ((l-fac (if (<= (floor (* factor l)) 20)
			  (- l 4)
			  (floor (* factor l))))
	       (end-arr (make-array l-fac :fill-pointer 0))
	       ;; die Dichte der Wiederholungen
	       (step (/ 1. factor))
	       (samp-i1 0)
	       (samp-i2 1)
	       (samp-i3 2)
	       (samp-i4 3)
	       (poli (make-poli (elt samps samp-i1)
				(elt samps samp-i2)
				(elt samps samp-i3)
				(elt samps samp-i4)))
	       (x 0))
	  (loop for i below l-fac
		do (if (= samp-i4 (1- l))
		       (progn (vector-push (funcall poli x) end-arr)
			      (incf x step))
		       (if (>= 3 x)
			   (progn (vector-push (funcall poli x) end-arr)
				  (incf x step))
			   (let ((inc-samp (- (ceiling x) 3)))
			     (progn (incf x (- 3 (ceiling x)))
				    (incf samp-i1 inc-samp)
				    (incf samp-i2 inc-samp)
				    (incf samp-i3 inc-samp)
				    (incf samp-i4 inc-samp)
				    ;;				(when (<= samp-i4 (1- l))
				    (setf poli (make-poli (elt samps samp-i1)
							  (elt samps samp-i2)
							  (elt samps samp-i3)
							  (elt samps samp-i4)));)
				    (vector-push (funcall poli x) end-arr)
				    (incf x step))))))
	  end-arr)
	samps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Crossfade
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; morph-sample: schafft Übergang zwischen zwei samplewerten:
;;;               => samp1: 0.5 samp2: 1 fac: 0.5 => 0.75
;;;

(defun morph-sample (samp1 samp2 fac)
    "interpoliert zwischen zwei samplewerten"
    (let ((dif (- samp2 samp1)))
      (+ samp1 (* fac dif))))

(defun morph-waveset (waveset-samps1 waveset-samps2 fac)
  "morph sample für zwei arrays an Samples"
  (let* ((l (length waveset-samps1))
	 (arr (make-array l :fill-pointer 0)))
    (loop for i below l
	  do (vector-push (morph-sample (elt waveset-samps1 i) (elt waveset-samps2 i) fac) arr))
    arr))

;;; start und end sind start und endpunkt in der Datei mit 0 als Anfang und 1 als Ende

(defun morph-wavesets (wavesets samp-arr1 samp-arr2 start end start-fac end-fac
		       &optional (curve #'lin-curve))
  (let ((l (length samp-arr1))
	(l-w (length wavesets)))
    (when (not (= l (length samp-arr2)))
      (error "Arrays have to be of equal size"))
    (let* ((end-arr (make-array l :fill-pointer 0))
	   (start-i (floor (* start (1- l-w))))
	   (end-i (floor (* end (1- l-w))))
	   (fac-curve (make-fac-arr l-w start-i end-i start-fac end-fac curve)))
      (loop for j below l-w
	    for i below 0
	    do (let* ((waveset-samps1 (get-waveset (elt wavesets j) samp-arr1))
		      (waveset-samps2 (get-waveset (elt wavesets j) samp-arr2))
		      (fac (elt fac-curve i)))
		 (setf end-arr
		       (concatenate 'vector end-arr
				    (morph-waveset waveset-samps1 waveset-samps2 fac)))))
      end-arr)))

;;;
;; Morphen von zwei Files
;;;

(defun morph-arrays (samp-arr1 samp-arr2 start end start-fac end-fac
		     &optional (curve #'lin-curve))
  ;;den längeren Array ermitteln
  (let* ((l1 (length samp-arr1))
	 (l2 (length samp-arr2))
	 (max-l (if (> l1 l2) l1 l2))
	 (min-l (if (< l1 l2) l1 l2))
	 (arr-list (sort (list samp-arr1 samp-arr2) #'(lambda (x y) (> (length x) (length y)))))
	 ;;die Kurve des Überganz zwischen den Files
	 (start-i (floor (* start (1- max-l))))
	 (end-i (floor (* end (1- max-l))))
	 (fac-curve (make-fac-arr max-l start-i end-i start-fac end-fac curve))
	 ;;End-array initialisieren
	 (end-arr (make-array max-l :fill-pointer 0)))
    (loop for i below max-l
	  for j = (mod i min-l)
	  do (vector-push (morph-sample (elt (car arr-list) i)
					(elt (cadr arr-list) j)
					(elt fac-curve i))
			  end-arr))
    end-arr))

;;;
;; Scale Waveset to value
;;;

;;;
;; max = 0.7
;; scaled-to = 0.9
;; 0.7 * x = 0.9
;; x = scaled-to / max

(defun scale-loudness (samp-arr val)
  (let* ((l (length samp-arr))
	 (max-val (get-loudest-sample samp-arr))
	 (scaler (/ val max-val))
	 (end-arr (make-array l :fill-pointer 0)))
    (loop for i below l
	  do (vector-push (* scaler (elt samp-arr i)) end-arr))
    end-arr))
