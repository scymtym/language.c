;;;; language.c.shared.parser.asd --- System definition for the shared.parser module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.shared.parser"
  :description "Grammar rules shared between \"proper\" C and the preprocessor"
  :license     "GPLv3" ; see COPYING for details
  :author      ("Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
                "Daniel Kochmański")

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                (:version "esrap"                         "0.17")
                (:version "parser.common-rules"           "0.3")
                (:version "parser.common-rules.operators" "0.3"))

  :components  ((:module     "parser"
                 :pathname   "src/shared/parser"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "variables")
                              (:file       "util")
                              (:file       "grammar"))))

  :in-order-to ((test-op (test-op "language.c.shared.parser/test"))))

(defsystem "language.c.shared.parser/test"
  :description "Unit tests for the language.c.shared.parser system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ("alexandria"

               (:version "fiveam" "1.4")
               "parser.common-rules/test"

               "language.c.shared.parser")

  :components ((:module     "parser"
                :pathname   "test/shared/parser"
                :components ((:file       "package")
                             (:file       "util")
                             (:file       "grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.c.shared.parser.test '#:run-tests)))
