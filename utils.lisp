;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; So 9. Jul 20:43:00 CEST 2023
;;;
;;; Utilities-file: contains the curves that are used for various processes
;;;                 and other useful tools
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-waveset)

;;;
;; List-operations
;;;

(defun replace-nth (lst i new)
  "replaces the nth element of a list non-destructivly"
  (cond ((null lst) nil)
	((= i 0) (cons new (replace-nth (cdr lst) (1- i) new)))
	(t (cons (car lst) (replace-nth (cdr lst) (1- i) new)))))

(defun every-nth (lst nth start-index &optional (counter 0))
  "gibt ab start-index jedes n-te Element einer Liste zurück"
  (cond ((null lst) nil)
	((and (= (mod counter nth) 0) (= start-index 0))
	 (cons (car lst) (every-nth (cdr lst) nth start-index (1+ counter))))
	((and (not (= (mod counter nth) 0)) (= start-index 0))
	 (every-nth (cdr lst) nth start-index (1+ counter)))
	(t (every-nth (cdr lst) nth (1- start-index) counter))))

(defun incf-list (lst &optional (accum 0))
  "addiert jeweils kumulativ jeden Wert einer Liste um den Vorigen" 
  (cond ((null lst) nil)
	(t (cons accum (incf-list (cdr lst) (+ accum (car lst)))))))

;;;
;; Bsp.: (interleave-lists '((1 2 3 4) (1 2 3) (1 2 3 4 5))) => (1 1 1 2 2 2 3 3 3 4 4 5)
;;;

(defun interleave-lists (lists)
  "Kombiniert die Werte mehrerer listen in einem, können unterschiedlich lang sein"
  (let* ((sort-list lists)
	 (max-length (car (sort (mapcar #'length sort-list) #'>))))
    (loop for i below max-length
	  append (loop for sub-list in lists
		       if (nth i sub-list)
			 collect (nth i sub-list)))))

;;;
;; append-to-nth: eine Liste mit wavesets-listen für jeden Kanal bei einem Kanal
;;                möchte ich hinten ein waveset anfügen
;;;

(defun append-to-nth (lst nth new-element)
  (when (>= nth (length lst))
    (error "Index is greater than list"))
  (if (nth nth lst)
      (replace-nth lst nth (append (nth nth lst) (list new-element)))
      (replace-nth lst nth (list new-element))))

;;;
;; Array-Manipulation:
;;;

(defun cat-arrays (arr-list)
  "führt beliebig viele arrays zusammen"
  (cond ((null arr-list) nil)
	(t (concatenate 'vector (car arr-list) (cat-arrays (cdr arr-list))))))

(defun make-fac-arr (lngth start-i end-i start-fac end-fac curve)
  "makes an array with a curve from start-fac to end-fac in the range between start-i and end-i"
  (let* ((end-arr (make-array lngth :fill-pointer 0))
	 (end-x (- end-i start-i)))
    (loop for i below lngth
	  do (cond ((and (>= i start-i) (<= end-i))
		    (vector-push (funcall curve start-fac end-fac end-x (- i start-i)) end-arr))
		   ((< i start-i)
		    (vector-push start-fac end-arr))
		   ((> i end-i)
		    (vector-push end-fac end-arr))))
    end-arr))

(defun vector-push-extend-nth (lst nth new-element)
  "in einer Liste mit arrays drin wird dem N-ten array hinten ein neues element angefügt"
  (when (>= nth (length lst))
    (error "Index is greater than list"))
  (let* ((new-element-vec (make-array 1 :initial-element new-element)))
    (if (nth nth lst)
	(replace-nth lst nth (concatenate 'vector (nth nth lst) new-element-vec))
	(replace-nth lst nth new-element-vec))))

(defun interleave-arrays (arrays)
  "Kombiniert die Werte mehrerer Arrays in einem, alle müssen gleich sein"
  (when (not (every #'(lambda (array) (= (length array) (length (car arrays)))) arrays))
    (warn "arrays not of equal size"))
  (let* ((arr-length (* (length arrays) (length (car arrays))))
	 (end-arr (make-array arr-length :fill-pointer 0)))
    (loop for i below (length (car arrays))
	  do (loop for sub-array in arrays
		   do (vector-push (elt sub-array i) end-arr)))
    end-arr))

;;;
;; curves
;;;

(defun lin-curve (start-val end-val x-end-val x)
  (+ (* (/ (- end-val start-val) x-end-val) x) start-val))

(defun sqr-curve (start-val end-val x-end-val x)
  (+ start-val (* (expt x 2) (/ (- end-val start-val) (expt x-end-val 2)))))

(defun exp-curve (start-val end-val x-end-val x)
  (* start-val (exp (* (/ (log (/ end-val
				  start-val))
			  x-end-val)
		       x))))

;;;
;; Propability-functions:
;;;

(defun lower-rand-val (x)
  "Takes the lower of two random values"
  (car (sort (list (random x) (random x)) #'<)))
