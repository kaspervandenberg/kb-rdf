;;;; trie/util

;;;; The dreaded assorted file of utilities without any structure

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defun mapcar-2values (function &rest arg-lists)
  "Apply `function` to the arguments from `arg-lists` and return two lists with the results."
  (labels ((mapcar-2values-acc (remaining-arg-lists v1-acc v2-acc)
	     (cond
	       ((notany (lambda (x) x) remaining-arg-lists) (values v1-acc v2-acc))
	       (t (let ((cars (mapcar #'car remaining-arg-lists))
			(cdrs (mapcar #'cdr remaining-arg-lists)))
		    (multiple-value-bind (v1 v2) (apply function cars)
		      (mapcar-2values-acc cdrs (cons v1 v1-acc) (cons v2 v2-acc))))))))
    (multiple-value-bind (v1 v2) (mapcar-2values-acc arg-lists nil nil)
      (values (reverse v1)
	      (reverse v2)))))
