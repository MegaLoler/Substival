(defpackage :substival
  (:use :cl :substival.util :substival.tree :substival.choice :enumerable :cl-arrows :extended-type-specifiers)
  (:export sub-eval
	   sub-eval-1))
(in-package :substival)

;; input-spec and output-spec are simple extended type specifications
;; expression is matched against input spec, input spec is destructured
;; and bindings are made to destructured expression elements
;; then the type output-spec is realized as enumeration of conforming objects
;; within the lexical environment of those bindings
;; if no match can be made to input-spec, return nil
;; like macroexpand-1 but non deterministic
(defun sub-eval-1 (input-spec output-spec expression)
  "Perform a single layer of substitution."
  (choice-point
   expression
   (if (< expression 3) (list (1+ expression) (1+ (1+ expression)))))) ; this is a placeholder for testing

;; return a tree of nondeterministic choice points
;; represents the process of iterative evaluation until
;; the expression no longer matches the input-spec
(defun sub-eval (input-spec output-spec expression)
  "Recursively evaluate until no more matches can be made."
  (if (typep expression 'choice-point)
      (choice-point
       (state expression)
       (with-enumerable
	 (do-enumerable (element (choices expression))
	   (yield (sub-eval input-spec output-spec element)))))
      (sub-eval input-spec output-spec
		(sub-eval-1 input-spec output-spec expression))))

;;  (first (to-list (choices (sub-eval nil nil 0))))

;; (map-enumerable #'print (states (sub-eval nil nil 0)))

;; (-> 
;;     (map-enumerable #'state (choices (sub-eval nil nil 0)))
;;      (to-list))

;; (to-list (states (choices (sub-eval nil nil 0))))

;; (choose-first-state (sub-eval nil nil 0))

;; (choose (sub-eval nil nil 0)
;;  ; (where-state #'evenp)
;;   (states)
;;   (to-list))

;; (-> (sub-eval nil nil 0)
;;     (choose-first)
;;     (choices-states)
;;     (to-list))

;; (to-list (map-yield (lambda (e) (state e)) (choices (sub-eval nil nil 0))))


;; (to-list (map-yield #'identity '(1 2 3)))

;; (choose-> (sub-eval nil nil 0)
;;   (take 5)
;;   (to-list))

;; (choose (sub-eval nil nil 0) 1)


;; (to-list (car (to-list (car (to-list (car (to-list (sub-eval nil nil 0))))))))

;; (-> (sub-eval nil nil 0)
;;     (select-many #'identity)
;;     (select-many #'identity)
;;     (select-many #'identity)
;;     (to-list))

;; (do-enumerable (n (with-enumerable
;; 		    (loop
;; 		       :for x :in '(1 2 3)
;; 		       :do (yield x))))
;;   (print n))
