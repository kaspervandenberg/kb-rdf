(defvar *bucket-size* 64)
(defvar *bucket-bitmask* #b111111)
(defvar *bucket-bitmask-length* 6)


(defclass INode ()
  ((main :initform nil)))

(defclass CNode ()
  ((filled-nodes :initform 0
		 :initarg :filled-nodes
                 :documentation
		 "Bitmask with a `#b1` when this `cnode` contains a subnode with 
                  `hierarchical-key-fragment` with the given bit and `#b0` when this `cnode` contains 
                  no such subnode")
   (pointers :initform (make-array '(0))
	     :initarg :pointers
	     :documentation
	     "Array with pointers to subnodes. The number of `#b1` bits upto the the bit for the 
              subnode's hash defines the position of the pointer in `pointers`.")
   (key-index :initform 0
	      :initarg :key-index
	      :documentation
	      "Which key of the `hierarchical-key` does this `cnode` use to order its subnodes?")
   (fragment-index :initform 0
		   :initarg :fragment-index
		   :documentation
		   "Which fragment of the key of the `hierarchical-key` does this `cnode` use to
                    order its subnodes?")))


(defun hash-fragment (cnode key)
  "Return the fragment of the hash code of `key` for a subnode of `cnode`."
  (logand *bucket-bitmask*
	  (ash (sxhash key)
	       (* -1 *bucket-bitmask-length*
		  (slot-value cnode 'fragment-index)))))

(defun hierarchical-key-hash-fragment (cnode hierarchical-key)
  "Return the hash-key-fragment of the `hierarchical-key` to select the subnode branch of `cnode`."
  (hash-fragment cnode (nth (slot-value cnode 'key-index) hierarchical-key)))

(defun hash-bit (hash)
  "Return the bitmask for `filled-nodes` that has the bit for `hash` '1' and the other bits '0'."
  (ash #b1 hash))

(defun hierarchical-key-bit (cnode hierarchical-key)
  "Return a bitmask for `filled-nodes` that has the bit for hierarchical-key-hash-fragment '1' and
   the other bits '0'."
  (hash-bit (hierarchical-key-hash-fragment cnode hierarchical-key)))

(defun hash-previous-nodes-mask (hash)
  "Return a bitmask for `filled-nodes` which selects all hashes that come before `hash`."
  (apply #'logior (loop
		     for i
		     from 0
		     below hash
		       collect (hash-bit i))))

(defun hierarchical-key-previous-nodes-mask (cnode hierarchical-key)
  "Return a bitmask for `filled-nodes` which selects all keys that come before `hierarchical-key`."
  (hash-previous-nodes-mask (hierarchical-key-hash-fragment cnode hierarchical-key)))

(defun hash-pointer-index (cnode hash)
  "Return the index in `pointers` of `cnode` for `hash`."
  (logcount (logand (hash-previous-nodes-mask hash)
		    (slot-value cnode 'filled-nodes))))

(defun hierarchical-key-pointer-index (cnode hierarchical-key)
  "Return the index in `pointers` of `cnode` for `hierarchical-key`."
  (logcount (logand (hierarchical-key-previous-nodes-mask cnode hierarchical-key)
		    (slot-value cnode 'filled-nodes))))

(defun lookup (cnode hierarchical-key)
  "Return the subnode of `cnode` via its hash key"
  (let* ((hash (hierarchical-key-hash-fragment cnode hierarchical-key))
	 (key-bit (logand (slot-value cnode 'filled-nodes)
			  (hash-bit hash)))
	 (index (hash-pointer-index cnode hash))
	 (ptrs (slot-value cnode 'pointers)))
    (if (and key-bit
	     (< index (length ptrs)))
	(aref ptrs (hash-pointer-index cnode hash))
	nil)))

(defun safe-subseq (sequence start &optional end)
  (let ((len (length sequence)))
    (if (< start len)
	(subseq sequence start (if end (min end len) len)))))



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

(let* ((prefix "http://rdf.kaspervandenberg.net/test/")
       (s1 (concatenate 'string prefix "subj1"))
       (p1 (concatenate 'string prefix "pred1"))
       (o1 (concatenate 'string prefix "obj1"))
       (t1 (list s1 p1 o1))
       (cn1 (make-instance 'cnode)))
	   (setf (slot-value cn1 'filled-nodes) (hierarchical-key-bit cn1 t1))
	   (setf (slot-value cn1 'pointers) #('ptr1))
	   (lookup cn1 t1))

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
