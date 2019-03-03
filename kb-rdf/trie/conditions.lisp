;;;; trie/conditions

;;;; The conditions that can be signaled by operations on Tries

;;;; © Kasper van den Berg, 2019

(in-package :net.kaspervandenberg.kb-rdf.trie)


(define-condition no-hash-bits-left (error)
  ((key :initarg :key
	:reader get-key
	:documentation
	"The key whose hash bits are depleted.")
   (node :initarg :node
	 :reader get-node
	 :documentation
	 "The node for which the key of a (future) child node, that is beyond
the maximum trie depth is accessed."))
  (:documentation
   "Trying to derive a `sub-key` and `bucket-index` without enough `hash-bits` left.

A `Key`'s `hash` has a limmited number of bits; i.e. `cl:sxhash` (in sbcl)
generates hashes of 61 (or sometimes 29) bits.  The maximum depth depends on
`bucket-size`.  Normally, a `no-hash-bits-left` condition is only signalled when
two different keys have a hash collision."))
