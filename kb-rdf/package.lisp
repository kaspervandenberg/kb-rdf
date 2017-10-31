;; Package to query an SPARQL endpoint
(defpackage #:kb-rdf
  (:use :cl)
  (:import-from :cl-rdfxml :parse-document)
  (:import-from :drakma :http-request)
  (:export :*sparql-endpoint*
           :*sparql-update-endpoint*
           :*default-prefixes*
           :sparql-query
           :sparql-update
           :query-list))

