;;;; protocol.lisp --- Protocol functions provided by the interface module.
;;;;
;;;; Copyright (C) 2019, 2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.c.interface)

(defvar *default-search-path*
  '(#+unix               #P"/usr/include/"
    #+linux              #P"/usr/include/linux/"

    #+(and linux x86-64) #P"/usr/include/x86_64-linux-gnu/"
    #+(and linux x86-64) #P"/usr/include/x86_64-linux-gnu/c++/9/"

    #+linux              #P"/usr/include/c++/9/tr1/"
    #+linux              #P"/usr/include/c++/9/"))

(defun make-environment (source &key search-path)
  (make-instance 'eval:include-environment
                 :file        source ; TODO this should initialize include-stack and included-files
                                        ; :search-path search-path
                 :include-stack (list source)
                 :included-files (let ((h (make-hash-table :test #'equal)))
                                   (setf (gethash source h) "hack")
                                   h)))

(defun make-system-environment (source
                                &key (toolchain-id (info:guess-toolchain-id))
                                     search-path)
  (let ((environment (make-environment source))
        (information (info:find-information toolchain-id)))
    (info:augment-environment! environment information)))

;;; Preprocessing

(defmethod preprocess ((source string)
                       &key (target      *standard-output*)
                            (toolchain-id nil toolchain-id-supplied?)
                            (search-path  nil search-path-supplied?)
                            (environment  (apply #'make-system-environment "<string>"
                                                 (append (when toolchain-id-supplied?
                                                           (list toolchain-id))
                                                         (when search-path-supplied?
                                                           (list search-path))))))
  (let* ((builder (make-instance 'language.c.preprocessor.model:builder))
         (ast     (language.c.preprocessor.parser:parse source builder))
         (tokens  (eval:evaluate ast '() environment)))
    (eval:output tokens target)))

(defmethod preprocess ((source pathname)
                       &key (target      *standard-output*)
                            (toolchain-id nil toolchain-id-supplied?)
                            (search-path  nil search-path-supplied?)
                            (environment  (apply #'make-system-environment source
                                                 (append (when toolchain-id-supplied?
                                                           (list toolchain-id))
                                                         (when search-path-supplied?
                                                           (list search-path))))))
  (let* ((builder (make-instance 'language.c.preprocessor.model:builder))
         (ast     (language.c.preprocessor.parser:parse source builder))
         (tokens  (eval:evaluate ast '() environment)))
    (eval:output tokens target)))

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

(defun repl (&key (input-stream   *standard-input*)
                  (output-stream  *standard-output*)
                  (prompt         (lambda (stream which)
                                    (let ((string (ecase which
                                                    (:initial      "* ")
                                                    (:continuation "> "))))
                                      (write-string string stream))))
                  (toolchain-id   (info:guess-toolchain-id) toolchain-id-supplied?)
                  (search-path    nil search-path-supplied?)
                  (environment    (apply #'make-system-environment "<repl>"
                                         (append (when toolchain-id-supplied?
                                                   (list :toolchain-id toolchain-id))
                                                 (when search-path-supplied?
                                                   (list :search-path search-path)))))
                  (handle-errors? t))
  (format output-stream "// ~:[Guessed~;Using~] toolchain: ~A~%"
          toolchain-id-supplied? toolchain-id)
  (format output-stream "// Search path:~%~@<//   ~@;~{~A~^~@:_~}~:>~%"
          (eval::search-path environment))
  (format output-stream "// ~:D builtin macro~:P~%"
          (hash-table-count (eval::entries environment)))
  (labels ((print-preprocessed (preprocessed)
             (format output-stream "// After preprocessing:~%")
             (let ((lines (loop :for line :in (split-sequence:split-sequence ; TODO undeclared dependency?
                                               #\Newline preprocessed :remove-empty-subseqs t)
                                :for words = (split-sequence:split-sequence-if
                                              (alexandria:rcurry #'member '(#\Space #\Tab)) line
                                              :remove-empty-subseqs t)
                                :when words :collect words)))
               (format output-stream "~@<// ~@;~{'~{~A~^ ~}'~^~@:_~}~:>" lines))
             (terpri output-stream))
           (once (input)
             (let* ((preprocessed (with-output-to-string (stream) ; TODO evaluate-to-string
                                    (preprocess input :environment environment
                                                      :target      stream)))
                    (output?      (notevery (lambda (character)
                                              (member character '(#\Space #\Tab #\Newline)))
                                            preprocessed))
                    (ast          (when output?
                                    (print-preprocessed preprocessed)
                                    (language.c.c.parser:parse
                                     preprocessed 'list ; (make-instance 'language.c.c.model::builder)
                                     :rule '(or language.c.c.parser::block-item
                                                language.c.c.parser:function-definition
                                                language.c.c.parser::translation-unit)))))

               (when output?
                 (princ ast output-stream)
                 (terpri output-stream)))))
    (loop (with-simple-restart (abandon-input "Abandon the current input.")
            (handler-bind ((error (lambda (condition)
                                    (when handle-errors?
                                      (princ condition output-stream)
                                      (fresh-line output-stream)
                                      (invoke-restart 'abandon-input)))))
              (let ((input (read-complete-input input-stream output-stream prompt)))
                (let ((start (get-internal-real-time)))
                  (unwind-protect
                       (time (once input))
                    (format output-stream "// Took ~,3F s~%"
                            (/ (- (get-internal-real-time) start)
                               internal-time-units-per-second))))))))))
