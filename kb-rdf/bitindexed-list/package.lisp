;;;; Bitindexed-list/package.lisp
;;;; Store upto `bucket-size` element in an array.  To save space empty elements
;;;; are ommitted and only valid pointers are stored.

;;;; © Kasper van den Berg, 2019
(defpackage :net.kaspervandenberg.kb-rdf.bitindexed-list
  (:use :common-lisp)
  (:shadow find add remove)
  (:export :bucket-size
	   :BitIndexed-List
	   :index-out-of-range
	   :element-not-empty
	   :element-not-found
	   :find
	   :add
	   :remove
	   :get-elements
	   :print-object
	   :containsP))
