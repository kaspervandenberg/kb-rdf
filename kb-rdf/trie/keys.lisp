;;;; trie/keys

;;;; Nodes in a hierarchical or standard (concurrent) hash trie a organised by
;;;; their `Key`.  `Keys` can be translated to a bucket index in a
;;;; `Bitindexed-List` and keys can be translated into a key useable at the next
;;;; level of the trie.

;;;; © Kasper van den Berg, 2019

(in-package :net.kaspervandenberg.kb-rdf.trie)


;;; Class definitions
(defclass Key ()
  ((val :initarg :key
	:reader get-key
	:documentation
	"The value of this Key")
   (hash :initarg :hash
	 :initform nil
	 :documentation
	 "Cached `sxhash` of `key-val`, when nil it can be calculated.")
   (hash-fragment-index :initarg :fragment
			:reader get-fragment-index
			:documentation
			"The level in the trie for which `bucket-index` is valid.")
   (bucket-index :initform nil
		 :documentation
		 "Cached index into the `CNode's` `Bitindexed-List`."))
  (:documentation
   "Common base of all keys in a Trie"))


(defclass Flat-Key (Key)
  ()
  (:documentation
   "Keys without any level of nesting as used in conventional hash-tries and
Prokopcks CTrie."))


(defclass Hierarchical-Key (Key)
  ((prefix :initarg :prefix
	   :initform nil
	   :reader get-prefix
	   :documentation
	   "List of key-values that are used in ancestor nodes.")
   (suffix :initarg :suffix
	   :initform nil
	   :reader get-suffix
	   :documentation
	   "Parts of the key that are use in ofspring nodes."))
  (:documentation
   "Keys that contain a hierarchical structure; for example paths in a traditional
file system."))



;;; Public methods
(defun get-bucket-index (key)
  "Retrieve the cached `bucket-index` or calculate it"
  (when (not (slot-value key 'bucket-index))
    (sb-ext:compare-and-swap (slot-value key 'bucket-index) nil (calc-bucket-index key)))
  (slot-value key 'bucket-index))


(defmethod print-object ((key Flat-Key) stream)
  (print-unreadable-object (key stream :type t :identity t)
    (prin1 `((:key ,(get-key key))
	     (:hash ,(get-hash key))
	     (bucket-index ,(get-bucket-index key))
	     (hash-as-mask ,(bucket-bits-in-hash key))))
    ))



;;; Private methods
(defun get-hash (key)
  "Retrieve the cached `hash` of `key` or calculate it"
  (when (not (slot-value key 'hash))
    (sb-ext:compare-and-swap (slot-value key 'hash) nil (cl:sxhash (get-key key))))
  (slot-value key 'hash))


(let* ((bucket-n-bits
	(floor (log bucket-size 2)))
       (bucket-bitmask
	(apply #'logior (loop for i
			   from 0
			   below bucket-n-bits
			   collect (ash 1 i)))))
  (defun calc-bucket-index (key)
    "Convert part of the `hash` to an index in a `Bitindexed-List`."
    (logand (ash (get-hash key)
		 (* -1 (get-fragment-index key) bucket-n-bits))
	    bucket-bitmask))

  (defun bucket-bits-in-hash (key)
    "Show which bits from `hash` are used to calculate `bucket-index`"
    (let ((show-in-hex-p (eql 0 (rem bucket-n-bits 4))))
      (let ((bits-per-char (if show-in-hex-p 4 1)))
	(let ((hash-string (write-to-string (get-hash key) :base (expt 2 bits-per-char))))
	  (let ((chars-per-fragment (/ bucket-n-bits bits-per-char))
		(len-hash (length hash-string)))
	    (let ((ancestor-fragments-n-chars (* (get-fragment-index key) chars-per-fragment)))
	      (let ((remaining-istart 0)
		    (fragment-iend (max (- len-hash ancestor-fragments-n-chars) chars-per-fragment)))
		(let ((fragment-istart (max (- fragment-iend chars-per-fragment) 0)))
		  (let ((remaining-iend fragment-istart)
			(ancestor-istart fragment-iend))
		    (let ((remaining (subseq hash-string remaining-istart remaining-iend))
			  (fragment (subseq hash-string fragment-istart fragment-iend))
			  (ancestor (subseq hash-string ancestor-istart)))
		      (format nil "~a-~a-~a" remaining fragment ancestor))))))))))))


