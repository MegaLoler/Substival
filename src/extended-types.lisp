(defpackage :extended-types
  (:use :cl)
  (:export extended-typep
	   extended-subtypep
	   extended-type-of
	   extended-supertype-of
	   extended-coerce
	   extended-subtype-coerce
	   destructure-with-type
	   subtype-with-bindings))
(in-package :extended-types)

;; extended type specs
;;
;; literals: quoted items
;; a literal type specifier specifies all objects for which `(equalp ,object ,type-spec) is true
;; a literal type is considered a subtype of the types specified by all type specifiers that specify it
;;
;; variables: items of the form `(var ,name ,type) or `(var ,name) which is equivalent to `(var ,name t)
;; variables are named type-specs. for the purposes of type checking, `(var ,name ,type) is equivalent to type
;;
;; ambiguous type: items of the form `(amb . type-specs)
;; an ambiguous type specifier may specify any type specified by any of a list of type-specs
;;
;; type structure: items of the form `(ts . type-specs)
;; a type structure specifies a list the types of whose elements are specified by the corresponding elements in type-specs
;;
;; native type specifier: anything else
;; a native type specifier is a type specifier recognized natively by common lisp



;; type definitions and predicates for extended type specifiers

(defvar *literal-type-spec-form-name* 'quote)
(defvar *variable-type-spec-form-name* 'var)
(defvar *ambiguous-type-spec-form-name* 'amb)
(defvar *type-structure-spec-form-name* 'ts)

(defun formp (expression form-name)
  "Whether `expression' is a list whose car is `form-name'."
  (and (typep expression 'list)
       (equalp form-name (car expression))))



(defun literal-type-specp (expression)
  "Whether `expression' is a literal type specifier."
  (formp expression *literal-type-spec-form-name*))

(defun variable-type-specp (expression)
  "Whether `expression' is a variable type specifier."
  (formp expression *variable-type-spec-form-name*))

(defun ambiguous-type-specp (expression)
  "Whether `expression' is an ambiguous type specifier."
  (formp expression *ambiguous-type-spec-form-name*))

(defun type-structure-specp (expression)
  "Whether `expression' is a type stucture specifier."
  (formp expression *type-structure-spec-form-name*))

(defun native-type-specp (expression)
  "Whether `expression' is a native type specifier."
  nil) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; gotto figuer this 1 out

(defun extended-type-specp (expression)
  "Whether `expression' is an extended type specifier.
Does not include native type specifiers.
For that see `type-specp'."
  (or (literal-type-specp expression)
      (variable-type-specp expression)
      (ambiguous-type-specp expression)
      (type-structure-specp expression)))

(defun type-specp (expression)
  "Whether `expression' is a type specifier.
Includes both native and extended type specifiers."
  (or (native-type-specp expression)
      (extended-type-specp expression)))



(deftype literal-type-spec ()
  '(satisfies literal-type-specp))

(deftype variable-type-spec ()
  '(satisfies variable-type-specp))

(deftype ambiguous-type-spec ()
  '(satisfies ambiguous-type-specp))

(deftype type-structure-spec ()
  '(satisfies type-stucture-specp))

(deftype native-type-spec ()
  '(satisfies native-type-specp))

(deftype extended-type-spec ()
  '(satisfies extended-type-specp))

(deftype type-spec ()
  '(satisfies type-specp))



;; constructors and accessors for extended type specifiers

(defun make-literal-type-spec (expression)
  "Return a literal type specifier that specifies the literal type of `expression'."
  `(,*literal-type-spec-form-name* ,expression))

(defun literal-type-spec-expression (literal-type-spec)
  "Return the expression whose literal type is specified by `literal-type-spec'."
  (declare (type literal-type-spec literal-type-spec))
  (cadr literal-type-spec))



(defun make-variable-type-spec (name &optional (type-spec t))
  "Return a named type specifier named `name' that specifies the type specified by `type-spec'."
  (declare (type symbol name))
  (declare (type type-spec type-spec))
  `(,*variable-type-spec-form-name* ,name ,type-spec))

(defun variable-type-spec-name (variable-type-spec)
  "Return the name of named type specifier `variable-type-spec'."
  (declare (type variable-type-spec variable-type-spec))
  (cadr variable-type-spec))

(defun variable-type-spec-type-spec (variable-type-spec)
  "Return the type specifier of the type specified by `variable-type-spec'."
  (declare (type variable-type-spec variable-type-spec))
  (caddr variable-type-spec))



(defun make-ambiguous-type-spec (type-specs)
  "Return an ambiguous type specifier that specifies any of the types specified by the type specifiers in the list `type-specs'."
  (proclaim `(type type-spec ,@type-specs))
  `(,*ambiguous-type-spec-form-name* ,@type-specs))

(defun ambiguous-type-spec-type-specs (ambiguous-type-spec)
  "Return a list of type specifiers that specify the types that may be specified by `ambiguous-type-spec'."
  (declare (type ambiguous-type-spec ambiguous-type-spec))
  (cdr ambiguous-type-spec))



(defun make-type-structure-spec (type-structure)
  "Return a type structure specifier that specifies `type-structure'."
  `(,*type-structure-spec-form-name* ,@type-structure))

(defun type-structure-spec-type-structure (type-structure-spec)
  "Return the type structure specified by `type-structure-spec'."
  (declare (type type-structure-spec type-structure-spec))
  (cdr type-structure-spec))



;; checking whether a type specifier specifies a subtype of the type specified by another type specifier

(defun subtypep-literal-literal (subtype-spec type-spec)
  "Whether the literal type specifier `subtype-spec' specfies a subtype of the type specified by literal type specifier `subtype-spec'."
  nil)

(defun extended-subtypep (subtype-spec type-spec)
  "Whether `subtype-spec' specifies a subtype of the type specified by `type-spec'."
  nil)

(defun extended-typep (expression type-spec)
  "Whether `expression' is of type specified by type-spec."
  (extended-subtypep (make-literal-type-spec expression) type-spec))



;; checking what type something is

(defun extended-supertype-of (expression-spec &optional super-spec)
  "Return a type specifier that specifies a supertype of the type specified by `expression-spec'.
Optionally assert that the type specified by the return value is a subtype of the type specified by `super-spec'."
  nil)

(defun extended-type-of (expression &optional super-spec)
  "Return a type specifier that specifies a type of which `expression' is an instance.
Optionally assert that the type specified by the return value is a subtype of the type specified by `super-spec'."
  (extended-supertype-of (make-literal-type-spec expression) super-spec))



;; extended type conversion

(defun extended-subtype-coerce (expression-spec target-spec &optional source-spec)
  "Convert `expression-spec' to a type specifier that specifies a subtype of the type specified by `target-spec'.
Optionally assert that `expression-spec' specifies a subtype of the type specified by `source-spec.'"
  nil)

(defun extended-coerce (expression target-spec &optional source-spec)
  "Convert `expression' to an instance of the type specified by `target-spec'.
Optionally assert that `expression' is an instance of a type specified by `source-spec.'"
  (extended-subtype-coerce (make-literal-type-spec expression) target-spec source-spec))



;; destructuring and binding

(defun destructure-with-type (subtype-spec type-spec)
  "Perfom a destructuring-bind on `subtype-spec' according to the structure of `type-spec'.
Return the bindings as a hashmap."
  nil)

(defun subtype-with-bindings (type-spec bindings)
  "Return a type-spec that is a subtype of `type-spec' by replacing the variables in `type-spec' with the corresponding bindings in the hashmap `bindings'."
  nil)
