;;;; language.c.toolchain-information.asd --- System definition for the toolchain-information module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.toolchain-information"
  :description "A system for storing and retrieving information about compilation environments."
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("language.c.preprocessor.evaluator")

  :components  ((:module     "toolchain-information"
                 :pathname   "src/toolchain-information"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "guess")
                              (:file       "registry")
                              (:file       "augment"))))

  :in-order-to ((test-op (test-op "language.c.toolchain-information/test"))))

(defsystem "language.c.toolchain-information/test"
  :description "Unit tests for the language.c.toolchain-information system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on  ((:version "fiveam" "1.4")

                "language.c.toolchain-information")

  :components  ((:module     "toolchain-information"
                 :pathname   "test/toolchain-information"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "guess"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:language.c.toolchain-information.test '#:run-tests)))
