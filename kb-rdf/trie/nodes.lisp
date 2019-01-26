;;;; trie/nodes

;;;; There are Tries with different properties: Immutable-Trie, CTrie, Hierarchical-*-Trie
;;;; These tries are composed from different types of nodes.  Trie-nodes.lisp defines the node
;;;; classes for the Tries.

;;;; Names for the nodes come from Aleksandar Prokopec "Cache-Aware Lock-Free Concurrent Hash Tries",
;;;; Prokopec's CTrie will be one of the center Trie's implelemented.

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


;;; Common Base
(defclass NodeBase ()
  ()
  (:documentation
   "Common base of all nodes in a Trie"))


;;; Base nodes
(defclass CNode (NodeBase)
  ((branches :initform (make-instance 'net.kaspervandenberg.kb-rdf.bitindexed-list:BitIndexed-List)
	     :initarg :branches
	     :reader get-branches
	     :documentation
	     "The branches of this `CNode` indexed by some bits of their hash.

Each branch is an other kind of node."))
  (:documentation
   "Node that stores a branching table to its subnodes.

`CNode` is immutable`."))


(defclass SNode (NodeBase)
  ((key :initarg :key
	:reader get-key
	:documentation
	"The key under which this node is indexed.

The key's hash determines the node's path to the Trie root.")
   (value :initform nil
	  :initarg :value
	  :reader get-value
	  :documentation
	  "The value associated with `key`.

Value can be `nil`."))
  (:documentation
   "Leave node that contains the user's data; i.e. a value and the key under which it is stored."))


(defclass INode (NodeBase)
  ((main :initform nil
	 :initarg :main
	 :reader get-main
	 :documentation
	 "Points to a Node (i.e. `NodeBase` subclass) or `nil`."))
  (:documentation
   "Mutable part of a CTrie.

Write actions to `main` should be thread safe, then the Trie can be mutated concurrently with other
mutations and read operations.  INodes are an important part of what makes Aleskandar Prokopec's
CTries concurrent; regular trie do not contain INodes."))


;;; Trie type mixins
(defclass Immutable-Trie-Mixin (NodeBase)
  ()
  (:documentation
   "Nodes of immutable Tries"))


(defclass Concurrent-Trie-Mixin (NodeBase)
  ()
  (:documentation
   "Nodes of CTries, Aleskandar Prokopec's Cache-Aware Lock-Free Concurrent Hash Tries"))


;;; Condition mixins
(defclass Tombed-Mixin (NodeBase)
  ((tomb-session-id :initform (gensym)
		    :initarg :id
		    :reader get-session-id
		    :documentation
		    "Identifies the Tombed subtree of which this node is part.

Multiple threads can simultaneously mark nodes as tombed.  The different sub-trees of tombed-nodes
should be distinguished, therefore all tombed-nodes that belong to the same sub-tree are marked with
the same `tomb-session-id`."))
  (:documentation
   "Marks nodes that are locked as a group.

Sometimes parts of the CTrie must be locked, updated, and released.  A `Tombed-Mixin` is a mixin
that marks nodes that are part of a locked group.  The algorithm will mark the nodes in the subtree
as tombed, collect them, and rebuild the subtree."))


;;; Class-Mixin combinations
(defclass CNode-ro (CNode Immutable-Trie-Mixin)
  ())

(defclass SNode-ro (SNode Immutable-Trie-Mixin)
  ())

(defclass INode-c (INode Concurrent-Trie-Mixin)
  ())

(defclass CNode-c (CNode Concurrent-Trie-Mixin)
  ())

(defclass SNode-c (SNode Concurrent-Trie-Mixin)
  ())
