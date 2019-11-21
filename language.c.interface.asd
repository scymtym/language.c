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
  :depends-on  ("language.c.cpp.parser"
                "language.c.cpp.evaluator"

                "language.c.c.parser")

  :components  ((:module     "interface"
                 :pathname   "src/interface"
                 :components ((:file       "package")
                              (:file       "protocol")))))
