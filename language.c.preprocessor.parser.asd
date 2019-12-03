;;;; language.c.preprocessor.parser.asd --- System definition for the preprocessor.parser module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.preprocessor.parser"
  :description "A parser for the C preprocessor language"
  :license     "GPLv3" ; see COPYING for details
  :author      ("Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
                "Daniel Kochmański")

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("alexandria"

                (:version "esrap"                    "0.17")
                (:version "parser.common-rules"      "0.3")
                "architecture.builder-protocol"

                (:version "language.c.shared.parser" (:read-file-form "version-string.sexp")))

  :components  ((:module     "parser"
                 :pathname   "src/preprocessor/parser"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")
                              (:file       "grammar"))))

  :in-order-to ((test-op (test-op "language.c.preprocessor.parser/test"))))

(defsystem "language.c.preprocessor.parser/test"
  :description "Unit tests for the language.c.preprocessor.parser system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ((:version "fiveam" "1.4")

               "language.c.preprocessor.parser"

               "language.c.shared.parser/test")

  :components ((:module     "parser"
                :pathname   "test/preprocessor/parser"
                :serial     t
                :components ((:file       "package")
                             (:file       "grammar"))))

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.c.preprocessor.parser.test '#:run-tests)))
