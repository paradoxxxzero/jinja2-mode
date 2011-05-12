(require 'sgml-mode)

(defvar jinja2-mode-hook nil)

(defvar jinja2-mode-map
  (let ((jinja2-mode-map (make-keymap)))
    (define-key jinja2-mode-map "\C-j" 'newline-and-indent)
    jinja2-mode-map)
  "Keymap for Jinja2 major mode")

(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . jinja2-mode))

(defconst jinja2-font-lock-indenting-keywords
  '(
    "if" "else" "for" "block" "filter" "with"
    "raw" "macro" "autoescape" "trans" "call"
    ;; Hydra specific
    "auth" "showonmatch" "errorproof"))

(defconst jinja2-font-lock-builtin-keywords
  '(
    "as" "autoescape" "debug" "extends"
    "firstof" "in" "include" "load"
    "now" "regroup" "ssi" "templatetag"
    "url" "widthratio" "elif" "true"
    "false" "none" "False" "True" "None"
    "loop" "super" "caller" "varargs"
    "kwargs" "break" "continue" "is"
    "do" "pluralize" "set" "from" "import"
    "context" "with" "without" "ignore"
    "missing" "scoped"))

(defconst jinja2-font-lock-functions-keywords
  '(
    "abs" "attr" "batch" "capitalize"
    "center" "default" "dictsort"
    "escape" "filesizeformat" "first"
    "float" "forceescape" "format"
    "groupby" "indent" "int" "join"
    "last" "length" "list" "lower"
    "pprint" "random" "replace"
    "reverse" "round" "safe" "slice"
    "sort" "string" "striptags" "sum"
    "title" "trim" "truncate" "upper"
    "urlize" "wordcount" "wordwrap" "xmlattr"
    ;; Hydra specific
    "date_format" "money_format"
    "money_format_no_currency" "sublength"
    "json" "percent_format" "person_title"
    "mail_format" "sort_by" "split"))

(defconst  jinja2-font-lock-comments
  `(
    (,(rx "{#"
	  (* whitespace)
	  (group
	   (*? anything)
	   )
	  (* whitespace)
	  "#}")
     . (1 font-lock-comment-face t))))

(defconst jinja2-font-lock-keywords-1
  (append
   jinja2-font-lock-comments
   sgml-font-lock-keywords-1))

(defconst jinja2-font-lock-keywords-2
  (append
   jinja2-font-lock-keywords-1
   sgml-font-lock-keywords-2))

(defconst jinja2-font-lock-keywords-3
  (append
   jinja2-font-lock-keywords-1
   jinja2-font-lock-keywords-2
   `(
     (,(rx "{{"
	  (* whitespace)
	  (group
	   (*? anything)
	   )
	  (*
	   "|" (* whitespace) (*? anything))
	  (* whitespace)
	  "}}") (1 font-lock-variable-name-face t))
     (,(rx  (group "|" (* whitespace))
	    (group (+ word))
	    )
      (1 font-lock-keyword-face t)
      (2 font-lock-warning-face t))
     (,(rx-to-string `(and (group "|" (* whitespace))
		       (group
			,(append '(or)
				 jinja2-font-lock-functions-keywords
				 ))))
      (1 font-lock-keyword-face t)
      (2 font-lock-function-name-face t)
      )
     (,(rx-to-string `(and word-start
	   (? "end")
	   ,(append '(or)
		    jinja2-font-lock-indenting-keywords
	    )
	   word-end)) (0 font-lock-keyword-face))
     (,(rx-to-string `(and word-start
	   ,(append '(or)
		    jinja2-font-lock-builtin-keywords
	    )
	   word-end)) (0 font-lock-builtin-face))

     (,(rx (or "{%" "%}")) (0 font-lock-function-name-face t))
     (,(rx (or "{{" "}}")) (0 font-lock-type-face t))
     (,(rx "{#"
	   (* whitespace)
	   (group
	    (*? anything)
	    )
	   (* whitespace)
	   "#}")
      (1 font-lock-comment-face t))
     (,(rx (or "{#" "#}")) (0 font-lock-comment-delimiter-face t))
    )))

(defvar jinja2-font-lock-keywords
  jinja2-font-lock-keywords-1)

(defvar jinja2-mode-syntax-table
  (let ((jinja2-mode-syntax-table (make-syntax-table)))
    jinja2-mode-syntax-table)
  "Syntax table for jinja2-mode")

(defun sgml-indent-line-num ()
  "Indent the current line as SGML."
  (let* ((savep (point))
	 (indent-col
	  (save-excursion
	    (back-to-indentation)
	    (if (>= (point) savep) (setq savep nil))
	    (sgml-calculate-indent))))
    (if (null indent-col)
	0
      (if savep
	  (save-excursion indent-col)
	indent-col))))

(defun jinja2-indent-line ()
  "Indent current line as WPDL code"
  (interactive)
  (beginning-of-line)
  (if (bobp)  ; Check begining of buffer
      (indent-line-to (sgml-indent-line-num))
    (let ((not-indented t) (indent-width 2) cur-indent (html-indentation (sgml-indent-line-num)))
      (if (looking-at "^[ \t]*{% *e\\(nd\\|lse\\)") ; Check close tag
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (if (looking-at (concat "^[ \t]*{% *.*?{% *end" (regexp-opt jinja2-font-lock-indenting-keywords)))
		  (progn
		    (setq cur-indent (current-indentation))
		    (message (format "Jinja_No1] jinja : %d sgml : %d" cur-indent html-indentation )))
		(if (looking-at (concat "^[ \t]*{% *" (regexp-opt jinja2-font-lock-indenting-keywords)))
		    (setq cur-indent (current-indentation))
		  (setq cur-indent (- (current-indentation) indent-width)))
	      (message (format "Jinja_end1] jinja : %d sgml : %d" cur-indent html-indentation )))
	      )
	    (if (< cur-indent 0)
		(setq cur-indent 0)))
	(if (looking-at "^[ \t]*</") ; Assume sgml end block trust sgml
	    (progn
	      (setq cur-indent html-indentation)
	      (message (format "SGML_?1] jinja : %d sgml : %d" cur-indent html-indentation )))
	  (save-excursion
	    (while not-indented
	      (forward-line -1)
	      (if (looking-at "^[ \t]*{% *end") ; Don't indent after end
		  (progn
		    (setq cur-indent (current-indentation))
		    (message (format "Jinja_end2] jinja : %d sgml : %d" cur-indent html-indentation ))
		    (setq not-indented nil))
		(if (looking-at (concat "^[ \t]*{% *.*?{% *end" (regexp-opt jinja2-font-lock-indenting-keywords)))
		    (progn
		      (setq cur-indent (current-indentation))
		      (message (format "Jinja_No] jinja : %d sgml : %d" cur-indent html-indentation ))
		      (setq not-indented nil))
		  (if (looking-at (concat "^[ \t]*{% *" (regexp-opt jinja2-font-lock-indenting-keywords))) ; Check start tag
		      (progn
			(setq cur-indent (+ (current-indentation) indent-width))
			(message (format "Jinja_open] jinja : %d sgml : %d" cur-indent html-indentation ))
			(setq not-indented nil))
		    (if (looking-at "^[ \t]*<") ; Assume sgml block trust sgml
			(progn
			  (setq cur-indent html-indentation)
			  (message (format "SGML_?] jinja : %d sgml : %d" cur-indent html-indentation ))
			  (setq not-indented nil))
		      (if (bobp) ; We don't know
			  (setq not-indented nil))))))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to html-indentation))))) ; If we didn't see an indentation hint, then allow no indentation

;;;###autoload
(define-derived-mode jinja2-mode html-mode  "jinja2"
  "Major mode for editing jinja2 files"
  :group 'jinja2
  ;; it mainly from sgml-mode font lock setting
  (set (make-local-variable 'font-lock-defaults)
       '((
	  jinja2-font-lock-keywords
	  jinja2-font-lock-keywords-1
	  jinja2-font-lock-keywords-2
	  jinja2-font-lock-keywords-3)
         nil t nil nil
         (font-lock-syntactic-keywords
          . sgml-font-lock-syntactic-keywords)))
  (set (make-local-variable 'indent-line-function) 'jinja2-indent-line))
(provide 'jinja2-mode)
