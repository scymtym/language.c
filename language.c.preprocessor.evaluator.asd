;;;; language.c.preprocessor.evaluator.asd --- System definition for the preprocessor.evaluator module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.preprocessor.evaluator"
  :description "An evaluator for the C preprocessor language"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                "language.c.preprocessor.model"
                "language.c.shared.parser"
                "language.c.preprocessor.parser")

  :components  ((:module     "evaluator"
                 :pathname   "src/preprocessor/evaluator"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "environment")
                              (:file       "constant-expression")
                              (:file       "evaluator"))))

  :in-order-to ((test-op (test-op "language.c.preprocessor.evaluator/test"))))

(defsystem "language.c.preprocessor.evaluator/test"
  :description "Unit tests for the language.c.preprocessor.evaluator system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ((:version "fiveam" "1.4")

               "language.c.preprocessor.evaluator")

  :components ((:module      "evaluator"
                :pathname    "test/preprocessor/evaluator"
                :serial      t
                :components  ((:file       "package")
                              (:file       "evaluator"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.c.preprocessor.evaluator.test '#:run-tests)))
