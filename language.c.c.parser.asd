;;;; language.c.c.parser.asd --- System definition for the c.parser module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.c.parser"
  :description "A parser for the C18 version of the C programming language"
  :license     "GPLv3" ; see COPYING for details
  :author      ("Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
                "Daniel Kochmański")

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("esrap"
                "parser.common-rules"
                "parser.common-rules.operators"
                "architecture.builder-protocol"

                "language.c.shared.parser")

  :components  ((:module     "parser"
                 :pathname   "src/c/parser"
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "util")
                              (:file       "grammar"))))

  :in-order-to ((test-op (test-op "language.c.c.parser/test"))))

(defsystem "language.c.c.parser/test"
  :description "Unit tests for language.c.c.parser system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ((:version "fiveam" "1.4")
               "parser.common-rules/test"

               "language.c.c.parser"

               "language.c.shared.parser/test")

  :components ((:module     "parser"
                :pathname   "test/c/parser"
                :components ((:file       "package")
                             (:file       "grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.c.c.parser.test '#:run-tests)))
