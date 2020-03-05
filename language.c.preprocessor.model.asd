;;;; language.c.preprocessor.model.asd --- System definition for the preprocessor.model module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(defsystem "language.c.preprocessor.model"
  :description "Syntax tree classes for the C preprocessor language"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("utilities.print-items"

                "architecture.builder-protocol")

  :components  ((:module     "model"
                 :pathname   "src/preprocessor/model"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "classes")
                              (:file       "builder"))))

  :in-order-to ((test-op (test-op "language.c.preprocessor.model/test"))))

(defsystem "language.c.preprocessor.model/test"
  :description "Unit tests for the language.c.preprocessor.model system"
  :license     "GPLv3" ; see COPYING for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :depends-on ((:version "fiveam" "1.4")

               "language.c.preprocessor.model")

  :components ()

  :perform    (test-op (operation component)
                ; (uiop:symbol-call '#:language.c.preprocessor.model.test '#:run-tests)
                       ))
