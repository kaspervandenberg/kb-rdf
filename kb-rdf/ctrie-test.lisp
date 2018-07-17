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



(require :asdf)
(asdf:load-system :uiop)


(defun display-ctrie (ctrie)
  (uiop:with-temporary-file (:pathname png-data :keep t)
    (let ((dot-process (uiop:launch-program `("/usr/bin/dot" "-Tpng" ,(concatenate 'string "-o" (namestring png-data))) :input :stream)))
      (let ((dot-program (uiop:process-info-input dot-process)))
	(format dot-program "digraph g {~%")
	(net.kaspervandenberg.kb-rdf.ctrie:print-dot ctrie dot-program)
	(format dot-program "}~%")
	(uiop:close-streams dot-process))
      (uiop:wait-process dot-process)
      (uiop:launch-program `("/usr/bin/display" ,(namestring png-data))))))


(let ((x (net.kaspervandenberg.kb-rdf.ctrie:add-intern
	  (make-instance 'net.kaspervandenberg.kb-rdf.ctrie:SNode :key 'one :value 1)
	  'two
	  (sxhash 'two)
	  1
	  2)))
  (display-ctrie x))