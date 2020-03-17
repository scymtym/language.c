;;;; guess.lisp --- Toolchain identity guessing.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.toolchain-information)

(defun guess-compiler-type ()
  #+linux "gcc")

(defun guess-compiler-version (compiler-type)
  (declare (ignore compiler-type))
  #+linux "9")

(defun guess-machine-type ()
  #+x86-64 "x86_64"
  #-x86-64 (string-downcase (machine-type)))

(defun guess-software-type ()
  #+linux "linux"
  #-linux (string-downcase (software-type)))

(defun guess-userland-type (compiler-type machine-type software-type)
  (declare (ignore compiler-type machine-type software-type))
  #+linux "gnu")

(defun guess-toolchain
    (&key (compiler-type    (guess-compiler-type))
          (compiler-version (guess-compiler-version
                             compiler-type))
          (machine-type     (guess-machine-type))
          (software-type    (guess-software-type))
          (userland-type    (guess-userland-type
                             compiler-type machine-type software-type)))
  "Guess and return toolchain information for the local machine.

   Return a list of five values: 1) a string designating the C
   compiler type 2) a string designating the C compiler version 3) a
   string indicating the machine type 4) a string indicating
   the (operating system) software type 5) a string indicating the
   userland type."
  (list compiler-type compiler-version
        machine-type software-type userland-type))

(defun make-toolchain-id (compiler-type compiler-version
                          machine-type software-type userland-type)  
  (format nil "~A-~A-~A-~A-~A"
          compiler-type compiler-version
          machine-type software-type userland-type))

(defun guess-toolchain-id
    (&rest args &key compiler-type compiler-version
                     machine-type software-type userland-type)
  "Guess and return a toolchain id for the local machine.

   Return a string of the form \"CT-CV-MT-ST-UT\" where
   CT is the compiler type
   CV is the compiler version
   MT is the machine type
   ST is the (operating system) software type
   UT is the userland type."
  (declare (ignore compiler-type compiler-version
                   machine-type software-type userland-type))
  (apply #'make-toolchain-id (apply #'guess-toolchain args)))
