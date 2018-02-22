(defpackage :substival
  (:use :cl :extended-types)
  (:export sub-eval
	   sub-eval-1))
(in-package :substival)

;; convert expression-spec from type specified by source-spec to type specified by target-spec
;; ie coerce expression-spec from the type of all expressions in the source language to the type of all expressions in the target language
;; translation here is just a case of type conversion
(defun sub-eval-1 (expression-spec source-spec target-spec)
  "Perform a single iteration of evaluation."
  (extended-subtype-coerce expression-spec target-spec source-spec))

(defun sub-eval (expression-spec source-spec target-spec)
  "Iteratively evaluate `expression-spec' until it no longer specifies a type that is also specified by `source-spec'."
  (let ((result (sub-eval-1 expression-spec source-spec target-spec)))
    (if result
	(sub-eval result source-spec target-spec)
	expression-spec)))
