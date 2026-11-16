(asdf:defsystem #:cl-waveset
  :description "library to perform waveset synthesis on audio files in non real time"
  :author "Jesper von Borstel"
  :licence "MIT"
  :depends-on (#:cl-libsndfile)
  :serial t
  :components ((:file "package")
               (:file "audioIO")
               (:file "audio-utils")
               (:file "utils")
               (:file "analize")
               (:file "waveset")))
