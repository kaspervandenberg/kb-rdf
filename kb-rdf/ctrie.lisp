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
	    "The value associated with `key`.  Value can be nil."))
  (:documentation
   "Contains the user's data; i.e the value and the key under which it is stored."))


(defclass Tombed-CNode ()
    ((branches :initform (make-instance 'net.kaspervandenberg.kb-rdf.bitindexed-list:BitIndexed-List)
	       :initarg :branches
	       :reader get-branches
	       :documentation
	       "The branches of this `Tombed-CNode` indexed by some bits from their hash.  Each branch
is a INode.")
     (tomb-session-id :initarg :tomb-session-id
		      :reader get-tomb-session-id
		      :documentation
		      "Multiple operations can simultaneously start tomb and rebuild in the same
sub-tree.  The tomb and rebuild process of the outer tree should should wait for the inner tree to
finish its tomb and rebuild process and then do the outer sub tree's tomb and rebuild process. This
prevents additions or removals that occur directly after the inner tree's tomb and rebuild to be
overwritten by old data from the outer tomb and rebuild process. `tomb-session-id` identifies all
nodes in the sub tree that where tombed as part of the same tomb-and-rebuild process.  When the
tombing encounters a child with a different `tomb-session-id`, it knows this sub tree is being
tombed by a different process and that this sub tree must first finish its `tomb-and-rebuild`
process before the outer tree can be tombed and rebuilt."))
  (:documentation
   "A CNode that has been locked, before any changes to it can be made its subtree must be tombed
and rebuilt")) 


(defclass Tombed-SNode ()
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
     (tomb-session-id :initarg :tomb-session-id
		      :reader get-tomb-session-id
		      :documentation
		      "Multiple operations can simultaneously start tomb and rebuild in the same
sub-tree.  The tomb and rebuild process of the outer tree should should wait for the inner tree to
finish its tomb and rebuild process and then do the outer sub tree's tomb and rebuild process. This
prevents additions or removals that occur directly after the inner tree's tomb and rebuild to be
overwritten by old data from the outer tomb and rebuild process. `tomb-session-id` identifies all
nodes in the sub tree that where tombed as part of the same tomb-and-rebuild process.  When the
tombing encounters a child with a different `tomb-session-id`, it knows this sub tree is being
tombed by a different process and that this sub tree must first finish its `tomb-and-rebuild`
process before the outer tree can be tombed and rebuilt."))
  (:documentation
   "A SNode that has been locked, before any changes to it can be made its subtree must be tombed and
rebuilt"))


(define-condition dangling-inode (error)
  ()
  (:documentation
   "We found a INode whose `main` is nil.  A parent node should clean it before continuing with the
current operation."))


(define-condition tombed-node (error)
  ((tomb-session-id :initarg :tomb-session-id
		    :reader get-tomb-session-id
		    :documentation
		    "Identifies the session of the encountered tombed-node.  The system should first
perform a tomb-and-rebuild for all descendants of this node with the same tomb-session-id."))
  (:documentation
   "We encountered a Tombed-node, before the requested add or remove operaterion can be executed, the
tombed subtree must be completely tombed and rebuilt."))


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


(define-condition key-not-found (error)
  ((key :initarg :key
	:reader get-key
	:documentation
	"The key that is attempted to remove from the CTrie and that the CTrie does not contain."))
  (:documentation
   "The CTrie does not contain the key that you attempt to remove."))


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


(defgeneric print-dot (node out)
  (:documentation
   "Render the node and its children to a graphviz graph."))


(defgeneric collect-subtree-values (node)
  (:documentation
   "Collect all SNodes in the CTrie rooted at `node`."))


(defgeneric tomb-node (node tomb-session-id)
  (:documentation
   "Change `node` into a tombed variant"))


(defgeneric tomb-children (node tomb-session-id)
  (:documentation
   "Recursively descend the subtree and tomb all nodes it contains."))


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
  (find-intern-cnode node key key-hash level))

(defmethod find-intern ((node Tombed-CNode) key key-hash level)
  (find-intern-cnode node key key-hash level))

(defun find-intern-cnode (node key key-hash level)
  (multiple-value-bind (child foundP) (cnode-find-child node key-hash level)
    (if foundP
	(find-intern child key key-hash (1+ level))
	(values child foundP))))


(defmethod find-intern ((node SNode) key key-hash level)
  (find-intern-snode node key key-hash level))

(defmethod find-intern ((node Tombed-SNode) key key-hash level)
  (find-intern-snode node key key-hash level))

(defmethod find-intern-snode (node key key-hash level)
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
  (multiple-value-bind (child foundP index) (cnode-find-child node key-hash level)
    (if (not foundP)
	(make-instance 'CNode :branches (net.kaspervandenberg.kb-rdf.bitindexed-list:add
					 (get-branches node)
					 index
					 (make-instance 'INode :main (make-instance 'SNode
										    :key key
										    :value value))))
	(progn
	  (add-intern child key key-hash (1+ level) value)
	  node))))


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


(defmethod add-intern ((node Tombed-CNode) key key-hash level value)
  (error 'tombed-node :tomb-session-id (get-tomb-session-id node)))


(defmethod add-intern ((node Tombed-SNode) key key-hash level value)
  (error 'tombed-node :tomb-session-id (get-tomb-session-id node)))


(defmethod remove-intern ((node INode) key key-hash level)
  (let ((m (get-main node)))
    (if (not m)
	(error 'dangling-inode))
    (handler-case
	(let ((success-result
	       (inode-cas-if-child-updated node #'remove-intern key key-hash level)))
	  (and (get-main node) success-result))	
      (dangling-inode ()
	(progn
	  (inode-cas-if-child-updated node #'cleanup)
	  (remove-intern node key key-hash level))))))


(defmethod remove-intern ((node CNode) key key-hash level)
  (restart-case
      (multiple-value-bind (child foundP index) (cnode-find-child node key-hash level)
	(if foundP
	    (if (remove-intern child key key-hash (1+ level))
		node
		(if (cnode-has-at-least-two-non-empty-branches node)
		    (make-instance 'CNode :branches (net.kaspervandenberg.kb-rdf.bitindexed-list:remove
						     (get-branches node)
						     index))
		    (tomb-node node (gensym))))))
    (keep-ctrie-as-is () node)))


(defmethod remove-intern ((node SNode) key key-hash level)
  (restart-case
      (if (equal key (get-key node))
	  nil
	  (error 'key-not-found :key key))
    (keep-ctrie-as-is () node)))


(defmethod remove-intern ((node Tombed-CNode) key key-hash level)
  (error 'tombed-node :tomb-session-id (get-tomb-session-id node)))


(defmethod remove-intern ((node Tombed-SNode) key key-hash level)
  (error 'tombed-node :tomb-session-id (get-tomb-session-id node)))


(defmethod print-object ((obj INode) out)
  (print-unreadable-object (obj out :type t)
    (format out "~%m:~a" (get-main obj))))


(defmethod print-object ((obj CNode) out)
  (print-object-cnode obj out))

(defmethod print-object ((obj Tombed-CNode) out)
  (print-object-cnode obj out))

(defun print-object-cnode (obj out)
  (print-unreadable-object (obj out :type t)
    (format out "br:~a" (get-branches obj))))


(defmethod print-object ((obj SNode) out)
  (print-object-snode obj out))


(defmethod print-object ((obj Tombed-SNode) out)
  (print-object-snode obj out))

(defun print-object-snode (obj out)
  (print-unreadable-object (obj out :type t)
    (format out "key:~a value:~a" (get-key obj) (get-value obj))))


(defmethod print-dot ((obj INode) out)
  (let ((m (get-main obj)))
    (if m
	(progn
	  (format out "~a [shape = Mcircle; width = 0.3; label = \"\"];~%~a -> ~a;~%"
		  (sxhash obj) (sxhash obj) (sxhash m))
	  (print-dot m out))
	(format out "~a [shape = Mcircle; label = \"\"];~%~a -> term~a;~% term~a [shape = plaintext; label = \"⏚\"] ;~%"
		(sxhash obj) (sxhash obj) (sxhash obj) (sxhash obj)))))


(defmethod print-dot ((obj CNode) out)
  (let ((brs (get-branch-elements-list obj)))
    (format out "~a [shape = plain; label =<<table><tr><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td><td>&nbsp;</td></tr></table>>];~%"
	    (sxhash obj))
    (loop for b in brs
       do (format out "~a -> ~a;~%"
		  (sxhash obj) (sxhash b))
       do (print-dot b out))))


(defmethod print-dot ((obj Tombed-CNode) out)
  (let ((brs (get-branch-elements-list obj)))
    (format out "~a [shape = plain; label = <<table><tr><td>╳</td><td>╳</td><td>╳</td><td>╳</td></tr></table>>];~%"
	    (sxhash obj))
    (loop for b in brs
       do (format out "~a -> ~a;~%"
		  (sxhash obj) (sxhash b))
       do (print-dot b out))))


(defmethod print-dot ((obj SNode) out)
  (format out "~a [shape = plain; label = <
<table>
  <tr><td border=\"0\">key:</td><td border=\"0\">~a</td></tr><hr/>
  <tr><td border=\"0\">hash:</td><td border=\"0\">0x~x</td></tr><hr/>
  <tr><td border=\"0\">val:</td><td border=\"0\">~a</td></tr>
</table>>];~%"
	  (sxhash obj) (get-key obj) (sxhash (get-key obj)) (get-value obj)))


(defmethod print-dot ((obj Tombed-SNode) out)
  (format out "~a [shape = plain; label = <
<table>
  <tr><td border=\"0\"><S>key:</S></td><td border=\"0\"><S>~a</S></td></tr><hr/>
  <tr><td border=\"0\"><S>hash:</S></td><td border=\"0\"><S>0x~x</S></td></tr><hr/>
  <tr><td border=\"0\"><S>val:</S></td><td border=\"0\"><S>~a</S></td></tr>
</table>>];~%"
	  (sxhash obj) (get-key obj) (sxhash (get-key obj)) (get-value obj)))


(defmethod collect-subtree-values ((node INode))
  (let ((m (get-main node)))
    (and m (collect-subtree-values m))))


(defmethod collect-subtree-values ((node CNode))
  (append (mapcar #'collect-subtree-values (get-branch-elements-list node))))


(defmethod collect-subtree-values ((node Tombed-CNode))
  (append (mapcar #'collect-subtree-values (get-branch-elements-list node))))


(defmethod collect-subtree-values ((node SNode))
  (list node))


(defmethod collect-subtree-values ((node Tombed-SNode))
  (list node))


(defmethod tomb-node ((node CNode) tomb-session-id)
  (make-instance 'Tombed-CNode :tomb-session-id tomb-session-id :branches (get-branches node)))


(defmethod tomb-node ((node SNode) tomb-session-id)
  (make-instance 'Tombed-SNode :tomb-session-id tomb-session-id :key (get-key node) :value (get-value node)))


(defmethod tomb-node ((node Tombed-CNode) tomb-session-id)
  (retomb-node node tomb-session-id))


(defmethod tomb-node ((node Tombved-SNode) tomb-session-id)
  (retorm-node node tomb-session-id))


(defun retomb-node (node tomb-session-id)
  (if (equal tomb-session-id (get-tomb-session-id node))
      node
      (error 'tombed-node :tomb-session-id (get-tomb-session-id node))))


(defmethod tomb-children ((node INode) tomb-session-id)
  (tomb-node (get-main node) tomb-session-id)
  (tomb-children (get-main node) tomb-session-id))


(defmethod tomb-children ((node CNode) tomb-session-id)
  (mapc (lambda (x) (tomb-children x tomb-session-id)) (get-branch-elements-list node)))


(defmethod tomb-children ((node SNode) tomb-session-id)
  node)


(defmethod tomb-children ((node Tombed-CNode) tomb-session-id)
  (if (equal tomb-session-id (get-tomb-session-id node))
      (mapc (lambda (x) (tomb-children x tomb-session-id)))
      (error 'tombed-node :tomb-session-id (get-tomb-session-id node))))


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


(defun cnode-find-child (cnode key-hash level)
  "Find the CNode's child with the given key-hash.
Returns three values: the child, a boolean that indicates whether the child was found and the index
of the child is or could be in the CNode's branches."
  (let ((index (key-hash-to-index key-hash level)))
    (multiple-value-bind (child foundP)
	(net.kaspervandenberg.kb-rdf.bitindexed-list:find (get-branches cnode) index)
      (values child foundP index))))


(defun key-hash-to-index (key-hash level)
  "Select the correct part of `key-hash` for `level`."
  (logand (ash key-hash
	       (* -1 level bucket-n-bits))
	  bucket-bitmask))


(defun cnode-has-at-least-two-non-empty-branches (node)
  (<= 2 (count-if #'(lambda (x) (get-main x)) (get-branches node))))


(defun get-branch-elements-list (node)
  (coerce (net.kaspervandenberg.kb-rdf.bitindexed-list:get-elements (get-branches node)) 'list))
