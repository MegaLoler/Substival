(defpackage :substival.util
  (:use :cl :enumerable)
  (:export erest
	   map-yield))
(in-package :substival.util)

(defun erest (enumerable)
  "Return all but the first element of an enumerable."
  (skip enumerable 1))

(defun map-yield (fn enumerable)
  "Return an enumerable that fields the result of applying a function to every element in an enumerable.
Like `mapcar' except for enumerables.
Like `map-enumerable' except collects the results as another enumerable."
  (with-enumerable
    (do-enumerable (element enumerable)
      (yield (funcall fn element)))))
