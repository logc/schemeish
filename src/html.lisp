(DEFPACKAGE #:SCHEMEISH.HTML
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.BASE #:MAP #:SORT #:STREAM)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.DEFINE #:LAMBDA)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.NAMED-LET #:LET)
  (:SHADOWING-IMPORT-FROM #:SCHEMEISH.SCHEMEISH
                          #:*
                          #:**
                          #:***
                          #:+
                          #:++
                          #:+++
                          #:-
                          #:/
                          #://
                          #:///)
  (:USE #:SCHEMEISH.SCHEMEISH))

(in-package #:schemeish.html)

(install-syntax!)

(defparameter *doctype* "<!doctype html>")

;; 6 kinds of elements
;; void: area, base, br, col, embed, hr, img, input, link, meta, param, source, track, wbr
;; template: template
;; raw text: script, style
;; escapable raw text: textarea, title
;; foreign elements: MathML, SVG
;; normal elements: everything else

(define (string->list string)
  (coerce string 'list))
(define (list->string list)
  (coerce list 'string))

(define (escape-char char)
  (cond
    ((char= char #\") "&quot;")
    ((char= char #\<) "&lt;")
    ((char= char #\&) "&amp;")
    (t (make-string 1 :initial-element char))))

(define (escape-text string)
  (apply 'string-append (map 'escape-char (string->list string))))

(define (escape-attribute-value value)
  (escape-text value))

(assert (equal? (escape-attribute-value "<&\"abc")
		"&lt;&amp;&quot;abc"))

(define (attribute-text name value)
  (string-append name "=\"" (escape-attribute-value value) "\""))

(assert (equal? (attribute-text "input" "do-no-evil")
		"input=\"do-no-evil\""))

(define (attrs-text attrs)
  (apply #'string-append (alist-map
			  attrs
			  (lambda (name value)
			    (if value
				(string-append " " (attribute-text name value))
				"")))))

(assert (equal? (attrs-text (alist "input" "valid" "output" ""))
		" input=\"valid\" output=\"\""))

(define (end-tag-text tag-name)
  (string-append "</" tag-name ">"))

(define (start-tag-text tag-name attrs)
  (string-append "<" tag-name (attrs-text attrs) ">"))

(assert (equal? (start-tag-text "a" (alist "href" "javascript:alert('<\"&\">');"
					   "name" "dumb"))
		"<a href=\"javascript:alert('&lt;&quot;&amp;&quot;>');\" name=\"dumb\">"))

(assert (equal? (end-tag-text "a")
		"</a>"))

(define (expr-text expr)
  [[expr :text]])

(defvar *expr?* (make-bundle-predicate :expr))
(define (expr? v) [*expr?* v])

(define (text string)
  (define (text)
    (escape-text string))
  (bundle *expr?* (text string)
	  text))

(assert (equal? (expr-text (text "body"))
		"body"))

(define (empty)
  (define (text) "")
  (bundle *expr?* (empty)
	  text))

(define (seq* exprs)
  (define (combined-exprs)
    (define (text)
      (apply 'string-append (map #'expr-text exprs)))
    (bundle *expr?* (seq* exprs)
	    text))

  (cond ((empty? exprs) (empty))
	((empty? (rest exprs)) (first exprs))
	(t (combined-exprs))))

(define (seq . exprs)
  (seq* exprs))

(define (doctype)
  (define (text) "<!doctype html>")
  (bundle *expr?* (doctype)
	  text))

(define (tag name attrs body)
  (define (text)
    (string-append
     (start-tag-text name attrs)
     (expr-text body)
     (end-tag-text name)))
  (define (tag-name) name)
  (define (tag-attrs) attrs)
  (define (tag-body) body)

  (bundle *expr?* (tag name attrs body)
	  text
	  tag-name
	  tag-attrs
	  tag-body))

(assert (equal? (expr-text (tag "a" (alist "href" "top" "name" "start") (text "body")))
		"<a href=\"top\" name=\"start\">body</a>"))

(define (document html)
  (seq (doctype) html))

(define (html head body)
  (tag "html" (alist "lang" "en") (seq head body)))

(define (head attrs title . meta-data-tags)
  (tag "head" attrs (seq title (seq* meta-data-tags))))

(define (list-item attrs expr)
  (tag "li" attrs expr))
(define (unordered-list attrs list-items)
  (tag "ul" attrs (seq* list-items)))
(define (ordered-list attrs list-items)
  (tag "ol" attrs (seq* list-items)))

(assert (equal? (expr-text (unordered-list (alist "class" "stylish")
					   (list
					    (list-item (alist "class" "stylish") (text "item 1"))
					    (list-item () (text "item 2"))
					    (list-item () (text "item 3")))))
		"<ul class=\"stylish\"><li class=\"stylish\">item 1</li><li>item 2</li><li>item 3</li></ul>"))

(assert (equal? (expr-text (ordered-list (alist "class" "stylish")
					 (list
					  (list-item (alist "class" "stylish") (text "item 1"))
					  (list-item () (text "item 2"))
					  (list-item () (text "item 3")))))
		"<ol class=\"stylish\"><li class=\"stylish\">item 1</li><li>item 2</li><li>item 3</li></ol>"))

;; Extend attribute lists

(define (global-attrs attrs
		      :access-key
		      :class
		      :content-editable
		      :text-direction
		      :draggable
		      :hidden
		      :id
		      :language
		      :spell-check
		      :style
		      :tab-index
		      :title
		      :translate)
  (alist-set* attrs
	      "accesskey" access-key
	      "class" class
	      "contenteditable" content-editable
	      "dir" text-direction
	      "draggable" draggable
	      "hidden" hidden
	      "id" id
	      "lang" language
	      "spellcheck" spell-check
	      "style" style
	      "tabindex" tab-index
	      "title" title
	      "translate" translate))

(define (private-attrs attrs custom-attrs)
  (apply 'alist-set* attrs
	 (alist-map custom-attrs
		    (lambda (name value) (cons (string-append "data-" name) value)))))

(assert (equal? (private-attrs () (alist "pizza" "pie" "not-pizza" "not-pie"))
		'((("data-pizza" . "pie") "data-not-pizza" . "not-pie"))))

(define (link-attrs attrs
		    :download
		    :href
		    :href-language
		    :media
		    :ping
		    :referrer-policy
		    :rel
		    :target
		    :media-type)
  (alist-set* attrs
	      "download" download
	      "href" href
	      "hreflang" href-language
	      "media" media
	      "ping" ping
	      "referrerpolicy" referrer-policy
	      "rel" rel
	      "target" target
	      "type" media-type))

(define (blockquote-attrs attrs :cite)
  (alist-set* attrs "cite" cite))

(define (blockquote attrs body)
  (tag "blockquote" attrs body))

(define ((make-simple-tag name) body)
  "TODO: This documentation should apply to the main function, not the closure.
Also todo, define inside of let should work."
  (tag name () body))

(define body (make-simple-tag "body"))
(define title (make-simple-tag "title"))

(define ((make-tag name) attrs body)
  (tag name attrs body))
(define div (make-tag "div"))

(define (sdiv* exprs)
  (div () (seq* exprs)))
(define (sdiv . exprs)
  (sdiv* exprs))

(define (system name packages)
  (sdiv name
	packages))

(define (packages . packages)
  (sdiv* packages))

(define (pkg name docs nicknames variables classes functions macros)
  (sdiv name
	docs
	nicknames
	variables
	classes
	functions
	macros))

(define (nicknames . nicknames)
  (sdiv (text "nick-names")
	(sdiv* nicknames)))
(define (nickname name)
  (sdiv name))

(define (variables . variables)
  (sdiv (text "variables")
	(sdiv* variables)))
(define (var name docs code-link xrefs)
  (sdiv name docs code-link xrefs))

(define (xrefs type . xrefs)
  (sdiv (text (string-append "Who " (symbol->string type)))
	(sdiv* xrefs)))
(define (xref function-name doc-link code-link)
  (sdiv function-name
	doc-link
	code-link))

(define (classes . classes)
  (sdiv (text "classes")
	(sdiv* classes)))
(define (closs name docs code-link supers slots)
  (sdiv name docs code-link supers slots))

(define (supers . supers)
  (sdiv (text "supers") (sdiv* supers)))
(define (super name doc-link code-link)
  (sdiv name doc-link code-link))

(define (slots . slots)
  (sdiv (text "slots") (sdiv* slots)))
(define (slot slot-name docs)
  (sdiv slot-name docs))

(define (functions . functions)
  (sdiv (text "functions") (sdiv* functions)))

(define (func name arguments docs examples code-link xrefs)
  (sdiv name arguments docs examples code-link xrefs))

(define (arguments . arguments)
  (sdiv (text "arguments") (sdiv* arguments)))
(define (argument name type)
  (sdiv name type))
(define (argument-type type)
  (sdiv (text (symbol->string type))))

(define (code-link symbol)
  (sdiv (text (string-append "CODE-LINK: " (symbol->string symbol)))))
(define (doc-link symbol)
  (sdiv (text (string-append "DOC-LINK: " (symbol->string symbol)))))

(define (examples . examples)
  (sdiv* examples))
(define (example code result)
  (sdiv code result))
(define (example-code code)
  (sdiv (text (format nil "~S" code))))
(define (example-result result)
  (sdiv (text (format nil "~S" result))))

(define (macros . macros)
  (sdiv* macros))

(define title (make-simple-tag "title"))

(define (variable->code-link symbol)
  (code-link symbol))
(define (function->code-link symbol)
  (code-link symbol))
(define (function->doc-link symbol)
  (doc-link symbol))

(define (who-references symbol)
  "List of function-name symbols that reference the variable referenced by symbol."
  (alist-keys (sb-introspect:who-references symbol)))

(define (variable->xrefs symbol)
  (apply 'xrefs
	 :references
	 (map (lambda (function-name)
		(xref (sdiv (text (symbol->string function-name)))
		      (function->doc-link function-name)
		      (function->code-link function-name)))
	      (who-references symbol))))

(define (uses-doctype)
  *doctype*)


(expr-text (variable->xrefs '*doctype*))

(expr-text (let ((symbol '*doctype*))
	     (let ((name (symbol-name symbol))
		   (docs (documentation symbol 'variable))
		   (code-link (variable->code-link symbol))
		   (xrefs (variable->xrefs symbol)))
	       (var (sdiv (text name))
		    (if (zero? (length docs))
			(empty)
			(sdiv (text docs)))
		    code-link
		    xrefs))))

(with-open-file (s "src/index.html" :direction :output :if-exists :supersede)
  (format s "~A" (expr-text
		  (document
		   (html
		    (head () (title (text "Title")))
		    (body
		     (system
		      (sdiv (text "schemeish system doc"))
		      (packages
		       (pkg
			(sdiv (text "package-name"))
			(sdiv (text "Package docs"))
			(nicknames
			 (nickname (text "package-nickname")))
			(variables
			 (var (sdiv (text "variable-name")) (sdiv (text "variable docs")) (code-link :variable-name)
			      (xrefs :references
				     (xref (sdiv (text "function-name")) (doc-link :function-name) (code-link :function-name)))))
			(classes
			 (closs (sdiv (text "class-name")) (sdiv (text "class documentation")) (code-link :class-name)
				(supers
				 (super (sdiv (text "function-name")) (doc-link :function-name) (code-link :function-name)))
				(slots
				 (slot (sdiv (text "slot-name")) (sdiv (text "docs"))))))
			(functions
			 (func (sdiv (text "function-name"))
			       (arguments (argument (sdiv (text "argument-name")) (argument-type :key)))
			       (sdiv (text "function-docs"))
			       (examples
				(example (example-code :code) (example-result :code)))
			       (code-link :function-name)
			       (xrefs :calls
				      (xref (sdiv (text "function-name")) (doc-link :function-name) (code-link :function-name)))))
			(macros
			 (func (sdiv (text "macro-name"))
			       (arguments (argument (sdiv (text "argument-name")) (argument-type :positional)))
			       (sdiv (text "macro-docs"))
			       (examples
				(example (example-code :code) (example-result :code)))
			       (code-link :macro-name)
			       (xrefs :macroexpands
				      (xref (sdiv (text "function-name")) (doc-link :function-name) (code-link :function-name))))))))))))))

(define (newline? char)
  "True if char is a newline character"
  (eq? char #\newline))
(define (whitespace? char)
  "True if char is a whitespace character."
  (member char '(#\tab #\newline #\linefeed #\page #\return #\space)))

(define (form-line-numbers stream)
  "Returns line-numbers associated with each top-level form in stream."
  (define (next) (read-char stream nil :eof))
  (define (peek) (peek-char nil stream nil :eof))
  (define (pos) (file-position stream))
  (define (eof? char) (eq? char :eof))
  
  (define (multi-comment-start?)
    "True if stream is at the start of a block comment #| ..."
    (let ((start (pos)))
      (prog1 (and (eq? (next) #\#)
		  (eq? (next) #\|))
	(file-position stream start))))
  (define (multi-comment-end?)
    "True if stream is at the end of a block comment #| ..."
    (let ((start (pos)))
      (prog1 (and (eq? (next) #\|)
		  (eq? (next) #\#))
	(file-position stream start))))
  
  (define (newlines-between start end)
    "The number of newlines in stream between the two file-positions."
    (let ((vec (make-array (- end start))))
      (file-position stream start)
      (read-sequence vec stream)
      (file-position stream end)
      (count #\newline vec)))

  (define (parse-line-comment line line-numbers)
    "Parses a ; comment up to newline."
    (let ((char (next)))
      (cond
	((eof? char) line-numbers)
	((newline? char) (parse (1+ line) line-numbers))
	(t (parse-line-comment line line-numbers)))))

  (define (parse-multi-comment line line-numbers)
    "Parses a multi-line-comment."
    (cond
      ;; TODO: handle nested multi-line comments?
      ((multi-comment-end?) (next) (next) (parse line line-numbers))
      (t (let ((char (next)))
	   (cond
	     ((eof? char) line-numbers)
	     ((newline? char) (parse-multi-comment (1+ line) line-numbers))
	     (t (parse-multi-comment line line-numbers)))))))
  
  (define (parse line line-numbers)
    (let ((char (peek)))
      (cond
	((eof? char) line-numbers)
	((newline? char)
	 (next)
	 (parse (1+ line) line-numbers))
	((whitespace? char)
	 (next)
	 (parse line line-numbers))
	((multi-comment-start?)
	 (next) (next)
	 (parse-multi-comment line line-numbers))
	((eq? char #\;) (parse-line-comment line line-numbers))
	(t (let ((start (pos)))
	     (read stream)
	     (let ((end (pos)))
	       (parse (+ line (newlines-between start end))
		      (cons line line-numbers))))))))

  (nreverse (parse 1 ())))

(define (find-definition object)
  (let ((source (sb-introspect:find-definition-source object)))
    (let ((path (sb-introspect:definition-source-pathname source)))
      (list path
	    (nth (first (sb-introspect:definition-source-form-path source))
		 (with-open-file (stream path)
		   (form-line-numbers stream)))))))

;; (find-definition #'html)

(uninstall-syntax!)
