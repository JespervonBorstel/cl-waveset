;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fr 7. Jul 11:53:38 CEST 2023
;;; Provides functions for audio input and output
;;; Works currently only with mono-wave-files as input.
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-waveset)

;; libsndfile's sf_open takes the mode as a plain C int (SFM_READ etc.),
;; not as a Lisp keyword.
(defconstant +sfm-read+  #x10)
(defconstant +sfm-write+ #x20)
(defconstant +sfm-rdwr+  #x30)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun read-audio-file-samples (filepath)
  "Opens an audio file using libsndfile and returns:
   1. A list of deinterleaved channel arrays (one float vector per channel).
   2. The sample rate.
   3. The number of channels."
  (cffi:with-foreign-objects ((sfinfo '(:struct cl-libsndfile:SF_INFO)))
    ;; Initialize sfinfo required fields to zero before opening for read
    (setf (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::frames) 0
          (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::samplerate) 0
          (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::channels) 0
          (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::format) 0
          (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::sections) 0
          (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::seekable) 0)

    (let ((infile (cl-libsndfile:sf_open (namestring filepath) +sfm-read+ sfinfo)))
      (when (cffi:null-pointer-p infile)
        (error "Failed to open audio file: ~A" filepath))

      (unwind-protect
           (let* ((frames (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::frames))
                  (samplerate (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::samplerate))
                  (channels (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::channels))
                  (total-items (* frames channels))
                  (interleaved-array (make-array total-items :element-type 'single-float
                                                                    :initial-element 0.0f0)))

             ;; Read interleaved frames into our flat buffer
             (cffi:with-pointer-to-vector-data (ptr interleaved-array)
               (cl-libsndfile:sf_readf_float infile ptr frames))

             ;; Deinterleave the flat buffer into a list of separate channel arrays
             (let ((channel-arrays (loop repeat channels
                                         collect (make-array frames :element-type 'single-float
                                                                    :initial-element 0.0f0))))
               (dotimes (f frames)
                 (dotimes (c channels)
                   (setf (aref (nth c channel-arrays) f)
                         (aref interleaved-array (+ (* f channels) c)))))

               (list channel-arrays samplerate)))

        ;; Ensure file handle is closed even if an error occurs
        (cl-libsndfile:sf_close infile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Output:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun resolve-cl-libsndfile-format (format-keyword bit-depth)
  "Combines a container format keyword (:wav, :flac, :ogg, :aiff) and bit-depth
   (16, 24, 32, or :float) into a valid libsndfile bitwise format mask."
  (let ((major (ecase format-keyword
                 (:wav  cl-libsndfile:sf_format_wav)
                 (:flac cl-libsndfile:sf_format_flac)
                 (:ogg  cl-libsndfile:sf_format_ogg)
                 (:aiff cl-libsndfile:sf_format_aiff)))
        (minor (if (eq format-keyword :ogg)
                   cl-libsndfile:sf_format_vorbis
                   (ecase bit-depth
                     (16     cl-libsndfile:sf_format_pcm_16)
                     (24     cl-libsndfile:sf_format_pcm_24)
                     (32     cl-libsndfile:sf_format_pcm_32)
                     (:float cl-libsndfile:sf_format_float)))))
    (logior major minor)))

(defun write-audio-file-samples (filepath channel-arrays samplerate
                                 &key (format :wav) (bit-depth 24))
  "Writes a list of float channel arrays to FILEPATH at SAMPLERATE.
   FORMAT can be :wav, :flac, :ogg, or :aiff.
   BIT-DEPTH can be 16, 24, 32, or :float (ignored for :ogg)."
  (when (null channel-arrays)
    (error "Cannot write audio file: channel-arrays list is empty."))

  (let* ((channels (length channel-arrays))
         (frames (length (first channel-arrays)))
         (total-items (* frames channels))
         (sf-format (resolve-cl-libsndfile-format format bit-depth)))

    ;; Verify all channels have identical frame lengths
    (unless (every (lambda (ch) (= (length ch) frames)) channel-arrays)
      (error "All channel arrays must have the exact same length."))

    (cffi:with-foreign-objects ((sfinfo '(:struct cl-libsndfile:SF_INFO)))
      ;; Set required fields for opening a file in write mode
      (setf (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::frames) 0
            (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::samplerate) samplerate
            (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::channels) channels
            (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::format) sf-format
            (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::sections) 0
            (cffi:foreign-slot-value sfinfo '(:struct cl-libsndfile:SF_INFO) 'cl-libsndfile::seekable) 0)

      ;; Validate format/channel/samplerate combination with libsndfile
      (when (zerop (cl-libsndfile:sf_format_check sfinfo))
        (error "Invalid libcl-libsndfile format combination: ~A at ~D-bit, ~D channels, ~D Hz"
               format bit-depth channels samplerate))

      (let ((outfile (cl-libsndfile:sf_open (namestring filepath) +sfm-write+ sfinfo)))
        (when (cffi:null-pointer-p outfile)
          (error "Failed to open output audio file for writing: ~A" filepath))

        (unwind-protect
             (let ((interleaved-array (make-array total-items :element-type 'single-float
                                                                      :initial-element 0.0f0)))
               ;; Interleave the separate channel arrays into a single buffer
               (dotimes (f frames)
                 (dotimes (c channels)
                   (setf (aref interleaved-array (+ (* f channels) c))
                         (coerce (aref (nth c channel-arrays) f) 'single-float))))

               ;; Write frames to disk
               (cffi:with-pointer-to-vector-data (ptr interleaved-array)
                 (let ((written (cl-libsndfile:sf_writef_float outfile ptr frames)))
                   (unless (= written frames)
                     (warn "Expected to write ~D frames, but only wrote ~D" frames written))
                   written)))

          ;; Always flush and close the file handle
          (cl-libsndfile:sf_close outfile))))))
