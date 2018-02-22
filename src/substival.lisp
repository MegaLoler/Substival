(defpackage :substival
  (:use :cl :extended-types)
  (:export sub-eval
	   sub-eval-1))
(in-package :substival)

;; input-spec and output-spec are extended type specifications
;; perform a destructuring bind on the expression according to the input-spec
;; then return a type-spec specifying a subtype of the output-spec
;; with variables in output-spec replaced with the destructured bindings
(defun sub-eval-1 (expression input-spec output-spec)
  "Perform a single iteration of evaluation."
  (subtype-with-bindings
   output-spec
   (destructure-with-type expression input-spec)))

(defun sub-eval (expression input-spec output-spec)
  "Iteratively evaluate `expression' until it is no longer specified by `input-spec'."
  (let ((result (sub-eval-1 expression input-spec output-spec)))
    (if result
	(sub-eval result input-spec output-spec)
	expression)))
