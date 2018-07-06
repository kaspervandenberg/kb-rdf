(let ((x (net.kaspervandenberg.kb-rdf.ctrie:add-intern
	  (make-instance 'net.kaspervandenberg.kb-rdf.ctrie:SNode :key 'one :value 1)
	  'two
	  (sxhash 'two)
	  1
	  2)))
  (format t "digraph g {")
  (net.kaspervandenberg.kb-rdf.ctrie:print-dot x t)
  (format t "}"))

(let ((x (net.kaspervandenberg.kb-rdf.ctrie:add-intern
	  (make-instance 'net.kaspervandenberg.kb-rdf.ctrie:SNode :key 'one :value 1)
	  'two
	  (sxhash 'two)
	  1
	  2)))
  (let ((brs (coerce
	      (net.kaspervandenberg.kb-rdf.bitindexed-list:get-elements
	       (net.kaspervandenberg.kb-rdf.ctrie:get-branches x))
	      'list)))
    (format nil "~a [shape = record; label =\"~{<~a>~^|~}\"];~%"
	    (sxhash x) (mapcar #'(lambda (x) (sxhash x)) brs))))
