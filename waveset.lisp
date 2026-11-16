;;;erstmal nur mit 16bit mono

(in-package :cl-waveset)

(defun push-waveset (waveset arr samp-arr)
  "fügt ein waveset hinten an den array an"
  (loop for i from (car waveset) to (cadr waveset)
	do (vector-push (elt samp-arr i) arr)))

(defun make-array-from-wavesets (wavesets samp-arr &key (zerocr-smoothing nil))
  "macht aus einem array von Wavesets wieder einen array"
  (let* ((arr (make-array (get-array-length wavesets) :fill-pointer 0)))
    (loop for i below (length wavesets)
	  do (push-waveset (elt wavesets i) arr samp-arr))
    (if zerocr-smoothing
	(smooth-zerocrossings wavesets arr)
	arr)))

;;; Funktion die zufällig Wavesets austauscht
;;; Achtung! destruktiv

(defun swap-elements (seq i j)
  (let* ((i-val (elt seq i))
	 (j-val (elt seq j)))
    (progn (setf (elt seq i) j-val)
	   (setf (elt seq j) i-val))
    seq))

(defun shuffle-n-times (seq n)
  (let* ((l (length seq))
	 (i (random l))
	 (j (random l)))
    (cond ((= n 0) seq)
	  ((= i j) (shuffle-n-times seq n))
	  (t (shuffle-n-times (swap-elements seq i j) (1- n))))))

(defun shuffle-wavesets (wavesets fac)
  (let* ((l (length wavesets))
	 (shuffles (floor (* fac l))))
    (shuffle-n-times wavesets shuffles)))

;;; Wiederholung aller Wavesets

(defun repeat-nth-waveset (wavesets n repeats)
  (let* ((end-arr (make-array (+ (length wavesets) (1- repeats)) :fill-pointer 0)))
    (loop for i from 0 below (length wavesets)
	  if (= i n)
	    do (loop repeat repeats
		     do (vector-push (elt wavesets i) end-arr))
	  else
	    do (vector-push (elt wavesets i) end-arr))
    end-arr))

(defun repeat-all-wavesets-n-times (wavesets repeats)
  (let* ((end-arr (make-array (* (length wavesets) repeats) :fill-pointer 0)))
    (loop for i from 0 below (length wavesets)
	  do (loop repeat repeats
		   do (vector-push (elt wavesets i) end-arr)))
    end-arr))

;;; ideen: wavesets unterschiedlich of wiederholen lassen
;;;	   nur einzelne wiederholen

;;;
;; Feature: Files Kombinieren, indem man abwechselnd Wavesets daraus liest
;; Vorgehen: der Lesearray wird alle samplearrays hintereinandergestellt sein
;;           => die indizes der Wavesets müssen dementsprechend aktualisiert werden
;;           => wichtig arrays und zugehörige Waveset-listen müssen in der gleichen Reihenfolge
;;              Vorkommen
;; Benötigt: Funktion, die waveset-listen interleaved,
;;           Funktion, die die Wavesets aus dem richtigen array liest
;;;

;;;
;; Bsp.: (update-indizes '(((0 2) (3 5) (6 8))
;;                         ((0 5) (6 20))
;;                         ((0 3) (4 6) (7 9) (10 12))))
;;       => (((0 2) (3 5) (6 8))
;;           ((9 14) (15 29))
;;           ((30 33) (34 36) (37 39) (40 42)))
;;;

(defun update-indizes (waveset-arrs)
  (let* ((adds (loop for waveset-arr in waveset-arrs
			     collect (cadr (elt waveset-arr (1- (length waveset-arr))))))
	 (waveset-adds (incf-list adds)))
    (loop for i from 0 below (length waveset-arrs)
	  and add in waveset-adds
	  collect (let* ((waveset-arr (nth i waveset-arrs))
			 (end-arr (make-array 0 :fill-pointer 0)))
		    (loop for j below (length waveset-arr)
			  do (vector-push-extend
			      (list (+ (first (elt waveset-arr j)) add i)
				    (+ (second (elt waveset-arr j)) add i))
			      end-arr))
		    end-arr))))

(defun interleave-waveset-arrays (waveset-arrays)
  (let* ((arr-lengths (sort (loop for waveset-arr in waveset-arrays
				  collect (length waveset-arr))
			    #'>))
	 (length-sum (loop for l in arr-lengths sum l))
	 (end-arr (make-array length-sum :fill-pointer 0)))
    (loop for i below (car arr-lengths)
	  do (loop for waveset-arr in waveset-arrays
		   if (< i (length waveset-arr))
		     do (vector-push (elt waveset-arr i) end-arr)))
    end-arr))

;;;
;; alles zusammenführen:
;;;

(defun combine-wavesets (waveset-arrs samp-arrays &key (zerocr-smoothing nil))
  "kombiniert beliebig viele Sounddateien,
   indem immer abwechselnd zirkulär wavesets daraus aufgerufen werden"
  (let* ((updated-waveset-arrs (update-indizes waveset-arrs))
	 (interleaved-wavesets (interleave-waveset-arrays updated-waveset-arrs))
	 (combined-array (cat-arrays samp-arrays))
	 (end-arr (make-array-from-wavesets interleaved-wavesets combined-array)))
    (if zerocr-smoothing
	(smooth-zerocrossings (analize-wavesets end-arr) end-arr)
	end-arr)))

;;;
;; Version die die Länge der der Files Berücksichtigt
;;;

;;;
;; nächstes Feature: Verteilung der einzelnen Wavesets auf unterschiedliche Kanäle
;; => einzelne Wavesets sind noch in der Original-anordnung im Raum verteilt
;;    make-array-fill-pointer argument benutzen
;;;

(defun thin-wavesets (wavesets samp-array i lngth)
  "schreibt nur wavesets ab i und alle Folgenden bis lngth"
  (let* ((chosen-wavesets (loop for j from i to (+ i (1- lngth)) collect (nth j wavesets)))
	 (output-arr (make-array (length samp-array) :initial-element 0.0)))
    (loop for waveset in chosen-wavesets
	  do (loop for k from (car waveset) to (cadr waveset)
		   do (setf (elt output-arr k) (elt samp-array k))))
    output-arr))

;;;
;; für mehrere Kanäle
;; lngth bestimmt, wie viele wavesets jeweils hintereinander aufgerufen werden pro Kanal
;; => wie sehr das original hörbar ist
;;;

(defun spread-wavesets-spatially (wavesets samp-array lngth chans)
  (let*
      ;; Liste der Waveset-indizes, bei denen die einzelnen Kanäle anfangen
      ;; bei z.b. 4 chans fängt der erste beim ersten Waveset an,
      ;; der zweite beim ersten viertel aller Wavesets usw.
      ((i-list (loop for i below chans
		     collect (floor (* (/ (length wavesets) chans) i)))))
    (loop for index in i-list
	  collect (thin-wavesets wavesets samp-array index lngth))))

;;;
;; Wavesets werden nach einer bestimmten sequenz, mehrkanalig zufällig gestreut
;; und zufällig ausgedünnt
;; => Kanal-Aufteilungsliste bsp. 8-ch: '(0 7 4 2 3 1 5 6) => erst ein Waveset auf Kanal 1,
;;                                      dann das nächste auf Kanal 8 usw im Kreis
;;;

;;;
;; dünnt waveset-list aus, nach einem wahrscheinlichkeitsfaktor zwischen
;; 0 und 1 wird ein waveset entfernt, bei 1 bleiben alle, bei 0 keins
;;;

(defun thin-wavesets-by-chance (wavesets chance)
  (let* ((rand-val (random 1.0)))
    (cond ((null wavesets) nil)
	  ((<= rand-val chance)
	   (cons (car wavesets) (thin-wavesets-by-chance (cdr wavesets) chance)))
	  (t (thin-wavesets-by-chance (cdr wavesets) chance)))))
	       
(defun thin-wavesets-by-chance-curve-1 (wavesets prob-arr)
  (let* ((end-arr (make-array 0 :fill-pointer 0 :element-type 'list)))
    (loop for i from 0 below (length wavesets)
	  and rand-val = (random 1.0)
	  if (< rand-val (elt prob-arr i))
	    do (vector-push-extend (elt wavesets i) end-arr))
    end-arr))

(defun thin-wavesets-by-chance-curve (wavesets start-i end-i start-chance end-chance
				      &optional (curve #'lin-curve))
  "entfernt wavesets aus einer Waveset-liste per Wahrscheinlichkeit"
  (let* ((prob-arr (make-fac-arr (length wavesets) start-i end-i
				    start-chance end-chance curve)))
    (thin-wavesets-by-chance-curve-1 wavesets prob-arr)))

;;;
;;  schreibt wavesets nicht in der Reihenfolge in der waveset-liste,
;;  sondern genau dahin, wo sie auch im Original sind
;;  => auch eine geshuffelte original-waveset-liste würde wieder exakt das original herausgeben
;;;

(defun make-array-from-wavesets-at-place (samp-array wavesets)
  (let* ((output-arr (make-array (length samp-array) :initial-element 0.0)))
    (loop for i below (length wavesets)
	  do (loop for k from (car (elt wavesets i)) to (cadr (elt wavesets i))
		   do (setf (elt output-arr k) (elt samp-array k))))
    output-arr))

(defun circle-wavesets-spatially (wavesets samp-array spatial-list chans)
  "verteilt wavesets zirkulär in Reihenfolge in der Spatial-list auf die angegebenen Kanäle"
  (let* ((all-wavesets
	   (let* ((init-all-wavesets
		    (loop repeat chans
			  collect (make-array 0 :fill-pointer 0 :element-type 'list))))
	     (loop for i below (length wavesets)
		   do (setf init-all-wavesets
			    (vector-push-extend-nth init-all-wavesets
						    (nth (mod i (length spatial-list))
							 spatial-list)
						    (elt wavesets i))))
			 init-all-wavesets)))
    (loop for wavesets-per-chan in all-wavesets
	  collect (make-array-from-wavesets-at-place samp-array wavesets-per-chan))))

;;;
;; Funktion:
;; Wavesets zu sawwave: samples innerhalb eines wavesets nach größe Sortieren
;;;

(defun sort-array-within-indices (samp-arr start-waveset end-waveset)
  "sortiert die Sample-Werte eines Wavesets absteigend, gibt das waveset als array zurück"
  (let* ((arr (make-array (1+ (- end-waveset start-waveset)) :fill-pointer 0)))
    (loop for i from start-waveset to end-waveset
	  do (vector-push (elt samp-arr i) arr))
    (sort arr #'>)))

(defun exchange-nth-waveset-with-sorted (wavesets samp-arr n)
  "macht das N-te Waveset zu einer saw-wave"
  (let* ((waveset (elt wavesets n))
	 (start-waveset (car waveset))
	 (end-waveset (cadr waveset))
	 (sorted-waveset (sort-array-within-indices samp-arr start-waveset end-waveset)))
    (loop for i from start-waveset to end-waveset
	  and j from 0
	  do (setf (elt samp-arr i) (elt sorted-waveset j)))
    samp-arr))

;;; Sollte mit (copy-seq) aufgerufen werden, da destruktiv

(defun sort-wavesets-by-chance-curve (wavesets samp-arr start-i end-i start-chance end-chance
				      &optional (curve #'lin-curve))
  "macht wavesets in einer Waveset-liste zu Saw-waves nach einer bestimmten Wahrscheinlichkeit"
  (let* ((prob-arr (make-fac-arr (length wavesets) start-i end-i
				    start-chance end-chance curve)))
    (loop for i from 0 below (length wavesets)
	  for rand-val = (random 1.0)
	  if (< rand-val (elt prob-arr i))
	    do (exchange-nth-waveset-with-sorted wavesets samp-arr i))
    samp-arr))

;;;
;; Jetzt dasselbe mit square-wave
;; höchstes und tiefstes
;;;

(defun square-array-within-indices (samp-arr start-waveset end-waveset)
  "macht aus einem Waveset eine Square-wave, gibt das waveset als array zurück"
  (let* ((waveset-samps (get-waveset (list start-waveset end-waveset) samp-arr))
	 (l (length waveset-samps))
	 (max-samp-val (loop for i below l maximize (elt waveset-samps i)))
	 (min-samp-val (loop for i below l minimize (elt waveset-samps i))))
    (loop for i below l
	  for samp = (elt waveset-samps i)
	  do (setf (elt waveset-samps i) (if (>= samp 0) max-samp-val min-samp-val)))
    waveset-samps))

(defun exchange-nth-waveset-with-squared (wavesets samp-arr n)
  "macht das N-te Waveset zu einer square-wave"
  (let* ((waveset (elt wavesets n))
	 (start-waveset (car waveset))
	 (end-waveset (cadr waveset))
	 (squared-waveset (square-array-within-indices samp-arr start-waveset end-waveset)))
    (loop for i from start-waveset to end-waveset
	  and j from 0
	  do (setf (elt samp-arr i) (elt squared-waveset j)))
    samp-arr))

;;; Sollte mit (copy-seq) aufgerzfen werden, da destruktiv

(defun square-wavesets-by-chance-curve (wavesets samp-arr start-i end-i start-chance end-chance
				      &optional (curve #'lin-curve))
  "macht wavesets in einer Waveset-liste zu square-waves nach einer
   bestimmten Wahrscheinlichkeit"
  (let* ((prob-arr (make-fac-arr (length wavesets) start-i end-i
				    start-chance end-chance curve)))
    (loop for i from 0 below (length wavesets)
	  for rand-val = (random 1.0)
	  if (< rand-val (elt prob-arr i))
	    do (exchange-nth-waveset-with-squared wavesets samp-arr i))
    samp-arr))

;;;
;; Funktion: zwischen Original Wellenform und Saw/irgendeiner anderen Wellenform Morphen
;;           solange beide Arrays derselben Länge sind
;;;

;;;
;; Funktion: Timestretching durch Wiederholung der Wavesets, so wie granular-timestretching
;;;

;;;
;; Mit optionalem Random. Ermöglicht über Angabe einer index-fun oder waveset-fun eine Auswahl,
;; welche wavesets gepitcht werden sollen
;; BSP.: (pitch-wavesets-by-parameter wavesets3 data-array3 0 1 2 2 2 2
;;                                    :curve #'exp-curve
;;				      :index-fun #'(lambda(i) (= (mod i 3) 0)))
;; => pitched jedes dritte Waveset zwei-oktaven nach unten
;; BSP.: (jittered-array (pitch-wavesets-by-parameter wavesets3 data-array3 0 1 10 10 10 10
;;			    :curve #'exp-curve
;;			    :waveset-fun #'(lambda (waveset samp-arr)
;;					   (< (get-waveset-loudness waveset samp-arr) 0.05)))
;; => alle wavesets mit RMS unter 0.05 werden um das 10-fache verlängert
;;;

(defun pitch-wavesets-by-parameter (wavesets samp-arr
				    start end
				    start-min-stretch end-min-stretch
				    start-max-stretch end-max-stretch
				    &key (curve #'lin-curve) index-fun waveset-fun)
  (when (not (or index-fun waveset-fun))
    (error "please provide a selection-function"))
  (when (and index-fun waveset-fun)
    (error "please provide only one selection-function"))
  (let* ((l (length wavesets))
	 (end-arr (make-array 0 :fill-pointer 0))
	 (start-i (floor (* start l)))
	 (end-i (floor (* end l)))
	 (min-arr (make-fac-arr l start-i end-i start-min-stretch end-min-stretch curve))
	 (max-arr (make-fac-arr l start-i end-i start-max-stretch end-max-stretch curve)))
    (if waveset-fun
	(loop for i below l
	      do (let* ((cur-max (elt max-arr i))
			(cur-min (elt min-arr i))
			(rand-val (+ cur-min (* (- cur-max cur-min)) (lower-rand-val 1.0)))
			(waveset (elt wavesets i))
			(waveset-arr (get-waveset waveset samp-arr))
			(stretched-waveset (stretch-array waveset-arr rand-val)))
		   (if (funcall waveset-fun waveset samp-arr)
		       (loop for j below (length stretched-waveset)
			     do (vector-push-extend (elt stretched-waveset j) end-arr)))
		       (loop for j below (length waveset-arr)
			     do (vector-push-extend (elt waveset-arr j) end-arr))))
	(loop for i below l
	      do (let* ((cur-max (elt max-arr i))
			(cur-min (elt min-arr i))
			(rand-val (+ cur-min (* (- cur-max cur-min)) (lower-rand-val 1.0)))
			(waveset (elt wavesets i))
			(waveset-arr (get-waveset waveset samp-arr))
			(stretched-waveset (stretch-array waveset-arr rand-val)))
		   (if (funcall index-fun i)
		       (loop for j below (length stretched-waveset)
			     do (vector-push-extend (elt stretched-waveset j) end-arr)))
		       (loop for j below (length waveset-arr)
			     do (vector-push-extend (elt waveset-arr j) end-arr)))))
    end-arr))

;;; kleine extra-Funktion: Alle wavesets gleich lang-gestretcht

(defun equalize-waveset-lengths (wavesets samp-arr freq
				 &key (zerocr-smoothing nil) (sample-rate 44100))
  (when (>= 0 freq)
    (error "freq can't be 0 or smaller"))
  (let* ((waveset-dur (/ 1 freq))
	 (waveset-samples (floor (* waveset-dur sample-rate)))
	 (l (length wavesets))
	 (end-arr (make-array (* waveset-samples l) :fill-pointer 0)))
    (loop for i below l
	  do (let* ((waveset (elt wavesets i))
		    (waveset-arr (get-waveset waveset samp-arr))
		    (waveset-l (get-waveset-length waveset))
		    (fac (/ waveset-samples waveset-l))
		    (stretched-waveset (stretch-array waveset-arr fac)))
	       (loop for j below (length stretched-waveset)
		     do (vector-push-extend (elt stretched-waveset j) end-arr))))
    (if zerocr-smoothing
	(smooth-zerocrossings (analize-wavesets end-arr) end-arr)
	end-arr)))

;;;
;; Funktion: waveset-shaking: Paare von Wavesets werdn so verkürzt oder verlängert,
;;                            dass sie trotzdem noch zusammen dieselbe länge haben
;;           BSP.: samp-l1 = 2000
;;                 samp-l2 = 350
;;                 l2 * fac2 = l2 + l1 - (l1 * fac1)
;;                 l2 * fac2 = l2 + (l1 - (l1 * fac1))
;;                      fac2 = (l2 + (l1 - (l1 * fac1))) / l2
;;;

(defun pair-shake-waveset (waveset-arr1 waveset-arr2 fac)
  (let* ((l1 (length waveset-arr1))
	 (l2 (length waveset-arr2))
	 (fac2 (if (<= (/ (+ l2 (- l1 (* l1 fac))) l2) 0)
		   0.01
		   (/ (+ l2 (- l1 (* l1 fac))) l2)))
	 (stretched-waveset1 (stretch-array waveset-arr1 fac))
	 (stretched-waveset2 (stretch-array waveset-arr2 fac2)))
    (concatenate 'vector stretched-waveset1 stretched-waveset2)))

(defun shake-wavesets (wavesets samp-arr fac)
  (let* ((end-arr (make-array 0 :fill-pointer 0)))
    (loop for i below (length wavesets) by 2
	  do (let* ((waveset1 (elt wavesets i))
		    (waveset2 (elt wavesets (1+ i)))
		    (waveset-arr1 (get-waveset waveset1 samp-arr))
		    (waveset-arr2 (get-waveset waveset2 samp-arr))
		    (shake-array (pair-shake-waveset waveset-arr1 waveset-arr2 fac)))
	       (loop for j below (length shake-array)
		     do (vector-push-extend (elt shake-array j) end-arr))))
    end-arr))

;;;
;; Funktion: Wavesetharmonisierung: Um vielfache Gepitchte Versionen des Wavesets
;;                                  drauf addiert und gewichtet
;;           harmonize-wavesets: extremely basic version. harmonizes all wavesets by a
;;                               fixed amount of harmonics.
;;                               Provides no control over loudness of harmonics 
;;                                
;;;

(defun make-one-harmonic (waveset-arr harm)
  (let* ((l (length waveset-arr))
	 (end-arr (make-array l :fill-pointer 0))
	 (harm-fac (/ 1 harm))
	 (stretched-arr (stretch-array waveset-arr harm-fac))
	 (stretched-l (length stretched-arr)))
    (loop for i below l
	  do (vector-push
	      (+ (* harm-fac (elt stretched-arr (mod i stretched-l)))
		 (elt waveset-arr i))
	      end-arr))
    end-arr))

(defun make-harmonics-til-i (waveset-arr i)
  (cond ((= i 1) waveset-arr)
	(t (make-harmonics-til-i (make-one-harmonic waveset-arr i) (1- i)))))

(defun harmonize-wavesets (wavesets samp-arr harmonics)
  (let* ((l-wavesets (length wavesets))
	 (l-samp-arr (length samp-arr))
	 (end-arr (make-array l-samp-arr :fill-pointer 0)))
    (loop for i below l-wavesets
	  do (let* ((waveset (elt wavesets i))
		    (waveset-arr (get-waveset waveset samp-arr))
		    (harmonised-waveset (make-harmonics-til-i waveset-arr harmonics))
		    (max-waveset-arr (get-loudest-sample waveset-arr))
		    (scaled-harm-arr (scale-loudness harmonised-waveset max-waveset-arr)))
	       (loop for j below (length scaled-harm-arr)
		     do (vector-push (elt scaled-harm-arr j) end-arr))))
    end-arr))

;;;
;; Funktion: Die Wavesets eines Klanges auf die Dauer der Wavesets eines anderen Stretchen
;;           Hat das erste File mehr Wavesets, wird in den neuen Wavesets wieder
;;           von vorne angefangen
;;           Andersherum wird der Rest der neuen Wavesets ignoriert
;;;

(defun stretch-to-different-wavesets (orig-wavesets samp-arr new-wavesets
				      &key (zerocr-smoothing nil))
  (let* ((l1 (length orig-wavesets))
	 (l2 (length new-wavesets))
	 (end-arr (make-array 0 :fill-pointer 0)))
    (loop for i below l1
	  do (let* ((wavesetold (elt orig-wavesets i))
		    (wavesetnew (elt new-wavesets (mod i l2)))
		    (l-wavesetold (get-waveset-length wavesetold))
		    (l-wavesetnew (get-waveset-length wavesetnew))
		    (waveset-arr (get-waveset wavesetold samp-arr))
		    (fac (/ l-wavesetnew l-wavesetold))
		    (stretched-waveset (stretch-array waveset-arr fac)))
	       (loop for j below (length stretched-waveset)
		     do (vector-push-extend (elt stretched-waveset j) end-arr))))
    (if zerocr-smoothing
	(smooth-zerocrossings (analize-wavesets end-arr) end-arr)
	end-arr)))

;;;
;; Funktion: Umdrehen einzelner Wavesets
;;;

;;;
;; Funktion: Arbeit mit Segmenten von Wavesets
;;;

;;;
;; Funktion: Morphen zwischen benachbarten Wavesets, Averaging
;;;

;;;
;; Funktion: Auswahl nach unterschiedlichen Analysekriterien
;;           - Länge (/ Pitch)
;;           - Lautstärke
;;           - Delta : Die Summe aller absuluten Änderungen zwischen den Samples
;;;
