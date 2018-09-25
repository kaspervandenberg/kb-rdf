;;;; trie/constants

;;;; The constants define the number of buckets in the CNode's of the tries.  The constants depend
;;;; On `net.kaspervandenberg.kb-rdf.bitindexed-list:bucket-size`


(in-package :net.kaspervandenberg.kb-rdf.trie)


(defconstant bucket-n-bits
  (floor (log net.kaspervandenberg.kb-rdf.bitindexed-list:bucket-size 2))
  "Number of bits to take from the key to index the bitindexed-list bukcet with.
`(expt 2 bucket-n-bits)` should be <= bucket-size as to not ovverflow the bitindexed-list.")


(defconstant bucket-bitmask
  (apply #'logior (loop for i
		     from 0
		     below bocket-n-bits
		     collect (ash 1 i)))
  "Bitmask used to extract the bits to use as index in the bitindexed-list from the key.")




