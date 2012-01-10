(in-package :cl-rewrite)

(defstruct (defstruct-definition (:conc-name struct-))
  name
  options
  slots)

(defstruct (defstruct-options (:conc-name do-))
  conc-name
  constructor
  copier
  include
  initial-offset
  named
  predicate
  printer
  type)

(defstruct (defstruct-slot-definition (:conc-name slot-))
  name
  initform
  (type t)
  read-only)

(defun parse-defstruct-options (name-and-options)
  (let ((name-and-options (alexandria:ensure-list name-and-options))
        options)
    (destructuring-bind (name &rest %options) name-and-options
      (setf options
            (apply #'make-defstruct-options (apply #'concatenate 'list %options)))
      (values name options))))

(defun parse-defstruct-slot-definition (slot-definition)
  (let ((slot-definition (alexandria:ensure-list slot-definition)))
    (destructuring-bind (name &optional initform &key type read-only) slot-definition
      (make-defstruct-slot-definition :name name
                                      :initform initform
                                      :type (or type t)
                                      :read-only read-only))))

;;; name of each slot vector := struct-name + slot-name // internalized
;;; struct object := fixnum
;;; (slot-accessor obj) := access the obj-th element of the slot vector
;;; (constructor ...) := set next-th value in every struct vector, use default value if not specified
;;; defstruct := create the following global variables
;;;  - current index (.index-of-struct-name.)
;;;  - .struct-name-slot-name. vectors
;;;  - constructor function
;;;  - slot accessor functions (inlined)
;;; reset function := reset index, vectors to initial values
(defun expander-for-defstruct (name-and-options slot-descriptions)
  (multiple-value-bind (name options)
      (parse-defstruct-options name-and-options)
    (let ((slot-definitions (mapcar #'parse-defstruct-slot-definition slot-descriptions)))
      (let* ((index (symbolicate ".INDEX-OF-" name "."))
             (constructor (or (do-constructor options) (symbolicate "MAKE-" name)))
             (slot-accessors (mapcar (lambda (s) (symbolicate name "-" (slot-name s)))
                                     slot-definitions))
             (slot-vectors (mapcar (lambda (s) (symbolicate "." name "-" (slot-name s) "."))
                                   slot-definitions)))
        `(progn
           ;;
           (declaim (type fixnum ,index))
           (defglobal ,index 0)
           ;;
           ,@(loop for sv in slot-vectors
                   for s in slot-definitions
                   collect `(declaim (type (simple-array ,(slot-type s) (*)) ,sv))
                   collect `(defglobal ,sv (make-array 1 :element-type ',(slot-type s))))
           ;;
           (declaim (inline ,constructor))
           (defun ,constructor (&key ,@(loop for s in slot-definitions
                                             collect (slot-name s)))
             (declare ,@(loop for s in slot-definitions
                              collect `(type ,(slot-type s) ,(slot-name s))))
             (let ((i ,index))
               (declare (array-index i))
               (labels ((extend-vectors ()
                          (when (>= i (length ,(first slot-vectors)))
                            ,@(loop for sv in slot-vectors
                                    for s in slot-definitions
                                    collect `(setf ,sv (replace (make-array (* 2 i) :element-type ',(slot-type s)) ,sv)))))
                        (assign-slot-values ()
                          ,@(loop for sv in slot-vectors
                                  for s in slot-definitions
                                  collect `(setf (aref ,sv ,index) (or ,(slot-name s) ,(slot-initform s))))))
                 (extend-vectors)
                 (assign-slot-values) 
                 (prog1
                     (the array-index i)
                   (incf ,index)))))
           ;;
           ,@(loop for sa in slot-accessors
                   for sv in slot-vectors
                   for s in slot-definitions
                   collect `(declaim (inline ,sa))
                   collect `(defun ,sa (,(symbolicate name "-OBJECT"))
                              (the ,(slot-type s) (aref ,sv ,(symbolicate name "-OBJECT")))))
           ;;
           (defun ,(symbolicate "RESET-" name) ()
             (setf ,index 0)
             ,@(loop for sv in slot-vectors
                     for s in slot-definitions 
                     collect `(setf ,sv (make-array 1 :element-type ',(slot-type s)))))
           )))))

(defmacro defstruct/v (name-and-options &rest slot-descriptions)
  (expander-for-defstruct name-and-options slot-descriptions))


;;; TODO:
;;; - conc-name-option
;;; - copier-option
;;; - include-option
;;; - initial-offset-option
;;; - named-option
;;; - predicate-option
;;; - printer-option
;;; - type-option

