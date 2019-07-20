;;;; language.c.cpp.model.asd --- System definition for the cpp.model module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.cpp.model"
  :description "Syntax tree classes for the C preprocessor language"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("utilities.print-items"

                "architecture.builder-protocol")

  :components  ((:module     "model"
                 :pathname   "src/cpp/model"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "classes")
                              (:file       "builder"))))

  :in-order-to ((test-op (test-op "language.c.cpp.model/test"))))

(defsystem "language.c.cpp.model/test"
  :description "Unit tests for the language.c.cpp.model system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ((:version "fiveam" "1.4")

               "language.c.cpp.model")

  :components ()

  :perform    (test-op (operation component)
                (uiop:symbol-call '#:language.c.cpp.parser.test '#:run-tests)))
