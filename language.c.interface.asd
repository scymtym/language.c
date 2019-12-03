;;;; language.c.interface.asd --- System definition for the interface module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.interface"
  :description "Convenience functions for parsing and evaluating C code"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("language.c.preprocessor.parser"
                "language.c.preprocessor.evaluator"

                "language.c.c.parser")

  :components  ((:module     "interface"
                 :pathname   "src/interface"
                 :components ((:file       "package")
                              (:file       "protocol"))))

  :in-order-to ((test-op (test-op "language.c.interface/test"))))

(defsystem "language.c.interface/test"
  :description "Unit tests for the language.c.interface system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on  ((:version "fiveam" "1.4")

                "language.c.interface")

  :components  ((:module     "interface"
                 :pathname   "test/interface"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:language.c.interface.test '#:run-tests)))
