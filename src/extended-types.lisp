(defpackage :extended-types
  (:use :cl)
  (:export extended-typep
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
;; type structure: items of the form `(type-structure . type-specs)
;; a type structure specifies a list the types of whose elements are specified by the corresponding elements in type-specs
;;
;; ambiguous type: items of the form `(amb . type-specs)
;; an ambiguous type specifier may specify any type specified by any of a list of type-specs
;;
;; native type specifier: anything else
;; a native type specifier is a type specifier recognized natively by common lisp

(defun extended-typep (expression type-spec)
  "Whether `expression' is an instance or subtype of the type specified by `type-spec'."
  nil)

(defun destructure-with-type (expression type-spec)
  "Perfom a destructuring-bind on `expression' according to the structure of `type-spec'.
Return the bindings as a hashmap."
  nil)

(defun subtype-with-bindings (type-spec bindings)
  "Return a type-spec that is a subtype of `type-spec' by replacing the variables in `type-spec' with the corresponding bindings in the hashmap `bindings'."
  nil)
