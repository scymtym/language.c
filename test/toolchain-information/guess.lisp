;;;; guess.lisp --- Unit tests for the toolchain identity guessing functions.
;;;;
;;;; Copyright (C) 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.toolchain-information.test)

(def-suite* :language.c.toolchain-information.gues
  :in :language.c.toolchain-information)

(test guess-toolchain.smoke
  "Smoke test for the `guess-toolchain' function."
  (let ((result (guess-toolchain)))
    (is (listp result))
    (is (= 5 (length result)))))

(test guess-toolchain-id
  "Smoke test for the `guess-toolchain-id' function."
  (let ((result (guess-toolchain-id)))
    (is (stringp result))))
