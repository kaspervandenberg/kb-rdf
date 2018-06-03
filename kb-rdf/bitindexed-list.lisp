;;;; Bitindexed-list.lisp
;;;; Store upto `bucket-size` element in an array.  To save space empty elements
;;;; are ommitted and only valid pointers are stored.
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
	   :print-object
	   :containsP))

(in-package :net.kaspervandenberg.kb-rdf.bitindexed-list)


(defconstant bucket-size 64)


(defclass BitIndexed-List ()
  ((filled-nodes :initform 0
		 :initarg :filled-nodes
		 :reader get-filled-nodes
		 :documentation
		 "Bitmask with a `#b1` when this `BitIndexed-List` contains an element with an 
index equal to the bit position and `#b0` when this `Bitindexed-List` does not contain an element 
for the index.")
   (elements :initform (make-array '(0))
	     :initarg :elements
	     :reader get-elements
	     :documentation
	     "The elements that this list contains."))
  (:documentation
   "Quickly access an element in the list without the storage taking much unnessecary space.
Note: a `BitIndexed-List` can maximally contain `bucket-size` elements.  A `BitIndexed-List is 
therefore not useable as a generic container, it can however be used as a node in an other
collection."))


(define-condition index-out-of-range (error)
  ((index-value :initarg :index-value)
   (allowed-range-min :initform 0
		      :initarg :allowed-range-min)
   (allowed-range-max :initform (- bucket-size 1)))
  (:documentation "The given index is beyond the allowed range of the `BitIndexed-List`"))

(define-condition element-not-empty (error)
  ((index-value :initarg :index-value)
   (current-element :initarg :current-element))
  (:documentation "The `BitIndexed-List` already has an element at the given index; there cannot be
two elements at the same index."))

(define-condition element-not-found (error)
  ((index-value :initarg :index-value))
  (:documentation "The `BitIndexed-List` does not have an element at the given index; it cannot be deleted."))
						 


(defun find (list index)
  "Return the element at `index` or `nil` when the list does not contain an element for `index`."
  (restart-case
      (and (containsP list index)
	   (aref (get-elements list) (element-index list index)))
    (not-found () nil)))


(defun add (list index new-value)
  "Return a copy of `list` that includes `newValue` at `index`."
  (restart-case
      (progn
	(if (containsP list index)
	    (error 'element-not-empty :index-value index :current-element (find list index)))
	(make-instance 'BitIndexed-List
		       :filled-nodes (set-filled-node list index)
		       :elements (concatenate 'vector
					      (elements-before list index)
					      (list new-value)
					      (elements-after list index))))
    (do-not-add () list)
    (replace-current-element ()
	:test (lambda (x) (typep x 'element-not-empty))
	(add (remove list index) index new-value))))


(defun remove (list index)
  "Return a copy of `list` without the element at `index`."
  (restart-case
      (progn
	(if (not (containsP list index))
	    (error 'element-not-found :index-value index))
	(make-instance 'BitIndexed-List
		       :filled-nodes (clear-filled-node list index)
		       :elements (concatenate 'vector
					      (elements-before list index)
					      (elements-after list index))))
    (return-list-as-is () list)))
						    


(defmethod print-object ((obj BitIndexed-List) out)
  (print-unreadable-object (obj out :type t)
    (format out "m:~b e:~a" (get-filled-nodes obj) (get-elements obj))))


;;; Internal functions

(defun containsP (list index)
  "Does `list` contain an element at `index`?"
  (if (< index 0) (error 'index-out-of-range :index-value index))
  (if (>= index bucket-size) (error 'index-out-of-range :index-value index))
  (not (eql 0
	    (logand (get-filled-nodes list)
		    (ash #b1 index)))))


(defun element-index (list index)
  "Convert `index` into an index in `elements` of `list`."
  (if (< index 0)
      (error 'index-out-of-range :index-value index))
  (if (>= index bucket-size)
      (error 'index-out-of-range :index-value index))
  (1- (logcount
       (logand (get-filled-nodes list)
	       (apply #'logior (loop
				  for i
				  from 0
				  upto index
				  collect (ash #b1 i)))))))


(defun elements-before (list index)
  "The suqsequence of elements in `list` before `index`"
  (let ((count-before (if (containsP list index)
			  (element-index list index)
			  (1+ (element-index list index)))))
    (if (> count-before 0)
	(subseq (get-elements list) 0 count-before))))


(defun elements-after (list index)
  "The subsequence of elements in `list` after `index`."
  (let ((pos (max (1+ (element-index list index)) 0)))
    (if (< pos (length (get-elements list)))
	(subseq (get-elements list) pos))))
		   
    
(defun set-filled-node (list index)
  "Set the bit for `index` in `filled-nodes` of `list`.
   Marking `list` as containing an element at `index`.  Returns a fresh bitmask."
  (if (< index 0)
      (error 'index-out-of-range :index-value index))
  (if (>= index bucket-size)
      (error 'index-out-of-range :index-value index))
  (logior (get-filled-nodes list)
	  (ash #b1 index)))


(defun clear-filled-node (list index)
  "Clear the bit for `index` in `filled-nodes` of `list`.
   Marking `list` as not containing an element at `index`.  Returns a fresh bitmask."
  (if (< index 0)
      (error 'index-out-of-range :index-value index))
  (if (>= index bucket-size)
      (error 'index-out-of-range :index-value index))
  (logandc2 (get-filled-nodes list)
	    (ash #b1 index)))
  
