(in-package :kb-rdf)

(defparameter *sparql-endpoint* "http://localhost:8080/openrdf-sesame/repositories/kbTest"
  "Default SPARQL endpoint to query.")

(defparameter *sparql-update-endpoint* "http://localhost:8080/openrdf-sesame/repositories/kbTest/statements"
  "Default SPARQL 1.1. Update endpoint")

(defparameter *content-negotiation-headers* 
  '(("Accept" . "application/rdf+xml")
    ("Content-type" . "application/x-www-form-urlencoded"))
  "Kind of content to send and receive. cl-rdfxml only accepts 'application/rdf+xml' 
   and 'application/x-www-form-urlencoded' is one of the types a sparql endpoint
   accepts as content type")

(defparameter *default-prefixes*
  '((:rdf . "http://www.w3.org/1999/02/22-rdf-syntax-ns\#")
    (:rdfs . "http://www.w3.org/2000/01/rdf-schema#\#")
    (:xsd . "http://www.w3.org/2001/XMLSchema#\#")
    (:fn . "http://www.w3.org/2005/xpath-functions#\#")
    (:owl . "http://www.w3.org/2002/07/owl#\#")
    (:dc . "http://purl.org/dc/elements/1.1/")))


(defun sparql-query (triple-receiver-f construct-query &key prefixes (endpoint *sparql-endpoint*))
  "Ask `construct-query` to the SPARQL `endpoint` call `triple-receiver-f` for each resulting triple.
   `triple-receiver-f` a call back function called with three arguments: subject, predicate, & object; see cl-rdfxml:parse-document."
  (check-type construct-query string "a SPARQL construct-query; e.g. 'construct { ?s ?p ?o. } where { ?s ?p ?o. }'")
  (check-type endpoint string "an URL to a sparql endpoint")
  (let ((rdf-reply (http-request endpoint :parameters (build-parameter-list "query" construct-query :prefixes prefixes) :additional-headers *content-negotiation-headers*)))
    (parse-document triple-receiver-f rdf-reply)))


(defun sparql-update (update-query &key prefixes (endpoint *sparql-update-endpoint*))
  "Update the RDF statements via `endpoint`."
  (check-type update-query string #.(format nil "a SPARQL update query; e.g.:~%   '~a~%    ~a~%~%    ~a~%    ~a~%    ~a~%    ~a~%    ~a'"
                                            "prefix ex: <http://www.example.org/ns/data#>"
                                            "prefix dc: <http://purl.org/dc/elements/1.1/>"
                                            "insert data"
                                            "{"
                                            "  ex:elem1 dc:title \"first demo element\";"
                                            "           dc:creator \"M. Lisper\"."
                                            "}"))
  (http-request endpoint 
                :parameters (build-parameter-list "update" update-query :prefixes prefixes) 
                :method :post 
                :additional-headers *content-negotiation-headers*))


(defun build-parameter-list (query-key query &key prefixes)
  "Construct a list to supply to `drakma:http-request` as `:parameters`"
  (list (cons query-key (format nil "~/KB-RDF::FORMAT-PREFIX-LIST/~&~a" prefixes query))))


(defun format-prefix (stream prefix &optional colon-p at-sign-p)
  "Format `prefix` as a SPARQL prefix declaration."
  (declare (ignore colon-p at-sign-p))
  (check-type prefix cons "a pair of symbol and uri prefix.")
  (check-type (car prefix) symbol "a symbol used as abbreviation for the uri prefix.")
  (check-type (cdr prefix) string "an uri prefix.")
  (format stream "prefix ~a: <~a>" 
          (string-downcase (symbol-name (car prefix)))
          (cdr prefix)))


(defun format-prefix-list (stream prefixes &optional colon-p at-sign-p)
  "Format a list of prefixes as SPARQL prefix declarations."
  (declare (ignore colon-p at-sign-p))
  (format stream "~{~/KB-RDF::FORMAT-PREFIX/~^~%~}" prefixes))
