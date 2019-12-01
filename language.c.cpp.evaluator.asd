;;;; language.c.cpp.evaluator.asd --- System definition for the cpp.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.cpp.evaluator"
  :description "An evaluator for the C preprocessor language"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "language.c.cpp.model"
                "language.c.shared.parser"
                "language.c.cpp.parser")

  :components  ((:module     "evaluator"
                 :pathname   "src/cpp/evaluator"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "environment")
                              (:file       "constant-expression")
                              (:file       "evaluator"))))

  :in-order-to ((test-op (test-op "language.c.cpp.evaluator/test"))))

(defsystem "language.c.cpp.evaluator/test"
  :description "Unit tests for the language.c.cpp.evaluator system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ((:version "fiveam" "1.4")

               "language.c.cpp.evaluator")

  :components ((:module      "evaluator"
                :pathname    "test/cpp/evaluator"
                :serial      t
                :components  ((:file       "package")
                              (:file       "evaluator"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.c.cpp.evaluator.test '#:run-tests)))
