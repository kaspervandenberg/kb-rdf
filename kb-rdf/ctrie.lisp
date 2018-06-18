;;;; CTrie.lisp
;;;; Store a collection of items indexed by their hash code.  The CTrie data structure is a
;;;; non-blocking concurrent hash array mapped trie based on single-word compare-and-swap instructions
;;;; in a shared-memory system. It supports concurrent lookup, insert and remove operations.
;;;; It follows the CTrie idea of Aleksandar Prokopec discussed in "Cache-Aware Lock-Free Concurrent
;;;; Hash Tries"
;;;;
;;;; This CTrie implementation is not yet of production quality, your are probably better of using
;;;; some other implementation such as cl-ctrie.
(defpackage net.kaspervandenberg.kb-rdf.ctrie
  (:use :common-lisp)
  (:shadow find add remove))

(in-package :net.kaspervandenberg.kb-rdf.ctrie)


(defconstant bucket-n-bits
  (floor (log net.kaspervandenberg.kb-rdf.bitindexed-list:bucket-size 2)))

(defconstant bucket-bitmask
  (apply #'logior (loop for i
	     from 0
	     below bucket-n-bits
	       collect (ash 1 i))))

(defclass INode ()
    ((main :initform nil
	   :initarg :main
	   :reader get-main
	   :documentation
	   "Points the `CNode`, `SNode`, or nil below this `INode`."))
  (:documentation
   "Indirection node, a key node in making the trie concurrent.  It remains in place when nodes
above or below it changes.  It allows atomic changing its sole subnode, `main`, via a
compare-and-swap."))


(defclass CNode ()
    ((branches :initform (make-instance 'net.kaspervandenberg.kb-rdf.bitindexed-list:BitIndexed-List)
	       :initarg :branches
	       :reader get-branches
	       :documentation
	       "The branches of this `CNode` indexed by some bits from their hash.  Each branch is a
INode."))
  (:documentation
   "Internal node that stores a branching table to its subnodes.  CNode is immutable."))


(defclass SNode ()
    ((key :initarg :key
	  :reader get-key
	  :documentation
	  "The key under which this node is indexed.  The key's hash determines the node's path to
the CTrie root.")
     (value :initform nil
	    :initarg :value
	    :reader get-value
	    :documentation
	    "The value associated with `key`.  Value can be nil.")
     (tome :initform nil
	   :initarg :tome
	   :reader tomeP
	   :documentation
	   "Flag to indicate this node is 'tomed' and should be removed."))
  (:documentation
   "Contains the user's data; i.e the value and the key under which it is stored."))


(define-condition dangling-inode (error)
  ()
  (:documentation
   "We found a INode whose `main` is nil.  A parent node should clean it before continuing with the
current operation."))


(define-condition duplicate-key (error)
  ((key :initarg :key
	:reader get-key
	:documentation
	"The key that is added to the CTrie and that is already in the CTrie.")
   (old-value :initarg :old-value
	      :reader get-old-value
	      :documentation
	      "The value that is already in the CTrie for `key`.")
   (new-value :initarg :new-value
	      :reader get-new-value
	      :documentation
	      "The value that is added to the CTrie."))
  (:documentation
   "The CTrie already contains the given key it can only contain each key once."))


(defgeneric find-intern (node key key-hash level)
  (:documentation
   "Search for `key` in `node` and its children.  If it is found, two values are returned: the
`value`in the `SNode` and `foundP` being `T`.  If the (sub-) tree does not contain `key`, `nil` and
`foundP` is `nil` are returned."))


(defgeneric add-intern (node key key-hash level value)
  (:documentation
   "Add `key`-`value` to the CTrie.  Return the possibly changed node that contains `key`-`value`"))


(defgeneric remove-intern (node key key-hash level)
  (:documentation
   "Remove `key` and its value from the CTrie."))


(defgeneric cleanup (node)
  (:documentation
   "Remove dangling INodes."))


(defmethod find-intern ((node INode) key key-hash level)
  (let ((m (get-main node)))
    (if (not m)
	(error 'dangling-inode))
    (handler-case
	(find-intern m key key-hash level)
      (dangling-inode ()
	(progn
	  (inode-cas-if-child-updated node #'cleanup)
	  (find-intern node key key-hash level))))))


(defmethod find-intern ((node CNode) key key-hash level)
  (multiple-value-bind (child foundP)
      (net.kaspervandenberg.kb-rdf.bitindexed-list:find (get-branches node)
							(key-hash-to-index key-hash level))
    (if foundP
	(find-intern child key key-hash (1+ level))
	(values child foundP))))


(defmethod find-intern ((node SNode) key key-hash level)
  (if (equal key (get-key node))
      (values (get-value node) T)))


(defmethod add-intern ((node INode) key key-hash level value)
  (let ((m (get-main node)))
    (if (not m)
	(error 'dangling-inode))
    (handler-case
	(inode-cas-if-child-updated node #'add-intern key key-hash level value)
      (dangling-inode ()
	(progn
	  (inode-cas-if-child-updated node #'cleanup)
	  (add-intern node key key-hash level value))))))


(defmethod add-intern ((node CNode) key key-hash level value)
  (let ((index (key-hash-to-index key-hash level)))
    (multiple-value-bind (child foundP)
	(net.kaspervandenberg.kb-rdf.bitindexed-list:find (get-branches node) index)
      (if (not foundP)
	  (make-instance 'CNode :branches (net.kaspervandenberg.kb-rdf.bitindexed-list:add
					   (get-branches node)
					   index
					   (make-instance 'INode :main (make-instance 'SNode
										      :key key
										      :value value))))
	  (progn
	    (add-intern child key key-hash (1+ level) value)
	    node)))))


(defmethod add-intern ((node SNode) key key-hash level value)
  (restart-case
      (if (not (equal key (get-key node)))
	  (add-intern (add-intern (make-instance 'CNode)
				  (get-key node)
				  (sxhash (get-key node))
				  level
				  (get-value node))
		      key key-hash level value)
	  (error 'duplicate-key :key key :old-value (get-value node) :new-value value))
    (keep-existing-value () node)
    (replace-value-with-new () (make-instance 'SNode :key key :value value))))


(defun inode-cas-if-child-updated (inode fupdate &rest update-args)
  "Apply `fupdate` to `inode`.`main`.  If `fupdate` returned an altered object, use cas to
atomivally update `inode`.`main`."
  (let ((m (get-main inode)))
    (let ((u (apply fupdate m update-args)))
      (if (not (eq m u))
	  (let ((old (sb-ext:compare-and-swap (slot-value inode 'main) m u)))
	    (if (eq m old)
		inode
		(inode-cas-if-child-updated inode fupdate update-args)))
	  inode))))


(defun key-hash-to-index (key-hash level)
  "Select the correct part of `key-hash` for `level`."
  (logand (ash key-hash
	       (* -1 level bucket-n-bits))
	  bucket-bitmask))
