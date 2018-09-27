;;;; trie/conditions

;;;; The conditions that can be signalled by operations on Tries

;;;; © Kasper van den Berg, 2018


(in-package :net.kaspervandenberg.kb-rdf.trie)


(define-condition dangling-inode (error)
  ((node :initarg :node
	 :reader get-node
	 :documentation
	 "The INode whose `main` is nil."))
  (:documentation
   "We found a INode whose `main` is nil.  A parent node should clean it before continuing with the
current operation."))
