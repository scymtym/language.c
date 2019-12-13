;;;; protocol.lisp --- Protocol functions provided by the interface module.
;;;;
;;;; Copyright (C) 2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.interface)

(defvar *default-search-path*
  '(#+unix               #P"/usr/include/"
    #+linux              #P"/usr/include/linux/"
    #+(and linux x86-64) #P"/usr/include/x86_64-linux-gnu/"))

(defun make-environment (source search-path)
  (make-instance 'language.c.preprocessor.evaluator:include-environment
                 :file        source ; TODO this should initialize include-stack and included-files
                 :search-path search-path
                 :include-stack (list source)
                 :included-files (let ((h (make-hash-table :test #'equal)))
                                   (setf (gethash source h) "hack")
                                   h)))

;;; Preprocessing

(defmethod preprocess ((source string)
                       &key (target      *standard-output*)
                            (environment (error "missing argument")))
  (let* ((builder (make-instance 'language.c.preprocessor.model:builder))
         (ast     (language.c.preprocessor.parser:parse source builder))
         (tokens  (language.c.preprocessor.evaluator:evaluate ast '() environment)))
    (language.c.preprocessor.evaluator::output tokens target)))

(defmethod preprocess ((source pathname)
                       &key (target      *standard-output*)
                            (search-path *default-search-path*)
                            (environment (make-environment source search-path)))
  (let* ((builder (make-instance 'language.c.preprocessor.model:builder))
         (ast     (language.c.preprocessor.parser:parse source builder))
         (tokens  (language.c.preprocessor.evaluator:evaluate ast '() environment)))
    (language.c.preprocessor.evaluator::output tokens target)))

;;; Parsing

(defmethod parse ((source t) &rest args &key search-path &allow-other-keys)
  (let* ((preprocessed-input (with-output-to-string (stream)
                               (apply #'preprocess source :target stream args)))
         (builder            'list      ; (make-instance 'language.c.)
                             )
         (ast                (language.c.c.parser:parse
                              preprocessed-input builder)))
    ast))

;;; Poor person's C REPL

(defun read-complete-input (input-stream output-stream prompt)
  (funcall prompt output-stream :initial)
  (loop :for line  = (read-line input-stream)
        :for input = (concatenate 'string (or input "") line (string #\Newline))
        :do (handler-case
                (progn
                  (language.c.preprocessor.parser:parse input 'list) ; TODO builder should be NIL
                  (return input))
              (error ()))
            (funcall prompt output-stream :continuation)))

(defun repl (&key (input-stream  *standard-input*)
                  (output-stream *standard-output*)
                  (prompt        (lambda (stream which)
                                   (let ((string (ecase which
                                                   (:initial      "* ")
                                                   (:continuation "> "))))
                                     (write-string string stream)))))
  (loop :with environment = (make-environment "<repl>" '())
        :for input        = (read-complete-input input-stream output-stream prompt)
        :for preprocessed = (with-output-to-string (stream)
                              (preprocess input :environment environment
                                                :target      stream))
        :for output?      = (notevery (lambda (character)
                                        (member character '(#\Space #\Tab #\Newline)))
                                      preprocessed)
        :for ast          = (when output?
                              (language.c.c.parser:parse preprocessed 'list
                                                         :rule '(or language.c.c.parser:function-definition
                                                                    language.c.c.parser::block-item)))
        :do (when output?
              (format output-stream "// After preprocessing:~%")
              (let ((lines (loop :for line :in (split-sequence:split-sequence ; TODO undeclared dependency?
                                                #\Newline preprocessed :remove-empty-subseqs t)
                                 :for words = (split-sequence:split-sequence-if
                                               (alexandria:rcurry #'member '(#\Space #\Tab)) line
                                               :remove-empty-subseqs t)
                                 :when words :collect words)))
                (format output-stream "~@<// ~@;~{'~{~A~^ ~}'~^~@:_~}~:>" lines))
              (terpri output-stream)

              (princ ast output-stream)
              (terpri output-stream))))
