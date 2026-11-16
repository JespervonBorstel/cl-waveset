cl-waveset

cl-waveset is a Common Lisp library for performing non-real-time waveset synthesis as decribed by Trevor Wishart.

Prerequisites & Installation

1. System Dependencies

Audio I/O is powered by libsndfile via CFFI. You must have the libsndfile C library installed on your operating system:

Debian / Ubuntu: sudo apt install libsndfile1-dev

Arch Linux: sudo pacman -S libsndfile

macOS (Homebrew): brew install libsndfile

2. Lisp Dependencies

cl-waveset depends on cl-libsndfile, which is not in the default Quicklisp dist. Clone both repositories into your Quicklisp local-projects directory:

cd ~/quicklisp/local-projects/
git clone https://github.com/ghollisjr/cl-libsndfile.git
# Ensure cl-waveset is also in ~/quicklisp/local-projects/ or registered with ASDF


3. Loading the System

In your Common Lisp REPL (e.g., SBCL with SLY or SLIME):

(ql:register-local-projects)
(ql:quickload #:cl-waveset)
(in-package #:cl-waveset)

4. Using cl-waveset

example.lisp provides examples and general usage tips for all waveset manipulations
in cl-waveset. Or have a look at the source code (comments are unfortunately in german).
