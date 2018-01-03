
(defvar *bucket-size* 64)

(defvar *bucket-bitmask* #b111111)
(defvar *bucket-bitmask-length* 6)


(defclass INode ()
  ((main :initform nil)))

(defclass CNode ()
  ((filled-nodes :initform 0
                 :documentation
		 "Bitmask with a `#b1` when this `cnode` contains a subnode with 
                  `hierarchical-key-fragment` with the given bit and `#b0` when this `cnode` contains 
                  no such subnode")
   (pointers :initform (make-array '(0))
	     :documentation
	     "Array with pointers to subnodes. The number of `#b1` bits upto the the bit for the 
              subnode's hash defines the position of the pointer in `pointers`.")
   (key-index :initform 0
	      :documentation
	      "Which key of the `hierarchical-key` does this `cnode` use to order its subnodes?")
   (fragment-index :initform 0
		   :documentation
		   "Which fragment of the key of the `hierarchical-key` does this `conde` use to
                    order its subnodes?")))

(defun hash-fragment (cnode key)
  "Return the fragment of the hash code of `key` for a subnode of `cnode`."
  (logand *bucket-bitmask*
	  (ash (sxhash key)
	       (* -1 *bucket-bitmask-length*
		  (slot-value cnode 'fragment-index)))))

(defun hierarchical-key-fragment (cnode hierarchical-key)
  "Return the key-fragment of the `hierarchical-key` to select the subnode branch of `cnode`."
  (hash-fragment cnode (nth (slot-value cnode 'key-index) hierarchical-key)))

(defun hierarchical-key-bit (cnode hierarchical-key)
  "Return a bitmask for `filled-nodes` that has the bit for hierarchical-key-fragment '1' and the 
   other bits '0'."
  (ash #b1 (hierarchical-key-fragment cnode hierarchical-key)))

(defun hierarchical-key-prev-mask (cnode hierarchical-key)
  "Return a bitmask for `filled-nodes` which selects all keys that come before `hierarchical-key`."
  (apply #'logior (loop
		     for i
		     from 0
		     below (hierarchical-key-fragment cnode hierarchical-key)
		       collect (ash #b1 i))))

(defun hierarchical-key-pointer-index (cnode hierarchical-key)
  "Return the index in `pointers` of `cnode` for `hierarchical-key`."
  (logcount (logand (hierarchical-key-prev-mask cnode hierarchical-key)
		    (slot-value cnode 'filled-nodes))))





;; Try out
(let ((i1 (make-instance 'INode))) (sb-ext:atomic-update (slot-value i1 'main) (lambda (_) 'test)) (print (slot-value i1 'main)))
(let ((key (list 'a 'b 'c))) key)

(defvar test-expr "http://example.org/rdf/test")
(format nil "~b~%~b~%~b"
	(sxhash test-expr)
	(hash-fragment test-expr 0)
	(hash-fragment test-expr 1))

(nth 0 '(resource "http://example.org/pred" "http://example.org/subj2"))

(let* ((prefix "http://rdf.kaspervandenberg.net/test/")
       (s1 (concatenate 'string prefix "subj1"))
       (p1 (concatenate 'string prefix "pred1"))
       (o1 (concatenate 'string prefix "obj1"))
       (t1 (list s1 p1 o1))
       (cn1 (make-instance 'cnode))))
(logcount 6)

(- (ash 1 (hash-fragment test-expr 0)) 1)

(ash 1 63)

(reduce)

(let ((test 'Hello world'))
  (defun kb-print-test ()
    (format test)))

(let ((prev-bits (make-array 64 :initial-contents
			      (loop for i from 0 below 64 collect
				   (concatenate 'bit-vector
						(loop for j from i below 64 collect #b0)
						(loop for j from 0 below i collect #b1))))))
  (flet ((display (i)
	   (format nil "~b~%" i)))
    (display (aref prev-bits 4))))



;; Reference
(subseq)
(concat)
