;;; includeplus.el -*- lexical-binding: t; -*-
;;; Code:
;;;
(defvar ip-plus-re "^[ \t]*#\\+eval:"
  "Include plus block main symbol regexp.")

(defun ip-update-include-plus (&optional included dir footnotes)
  "update every include keyword plus in buffer.
Optional argument INCLUDED is a list of included file names along
with their line restriction, when appropriate.  It is used to
avoid infinite recursion.  Optional argument DIR is the current
working directory.  It is used to properly resolve relative
paths.  Optional argument FOOTNOTES is a hash-table used for
storing and resolving footnotes.  It is created automatically."
  (let ((includer-file (buffer-file-name (buffer-base-buffer)))
	(case-fold-search t)
	(file-prefix (make-hash-table :test #'equal))
	(current-prefix 0)
	(footnotes (or footnotes (make-hash-table :test #'equal)))
	(include-re "^[ \t]*#\\+INCLUDE:"))
    ;; Expand INCLUDE keywords.
    (goto-char (point-min))
    (while (re-search-forward include-re nil t)
      (when (and (not (org-in-commented-heading-p))
                 (ip-is-plus-p))
	(let ((element (save-match-data (org-element-at-point))))
	  (when (eq (org-element-type element) 'keyword)
	    (beginning-of-line)
	    ;; Extract arguments from keyword's value.
	    (let* ((value (org-element-property :value element))
		   (ind (current-indentation))
		   location
		   (coding-system-for-read
		    (or (and (string-match ":coding +\\(\\S-+\\)>" value)
			     (prog1 (intern (match-string 1 value))
			       (setq value (replace-match "" nil nil value))))
			coding-system-for-read))
		   (file
		    (and (string-match "^\\(\".+?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)"
				       value)
			 (prog1
			     (save-match-data
			       (let ((matched (match-string 1 value)))
				 (when (string-match "\\(::\\(.*?\\)\\)\"?\\'"
						     matched)
				   (setq location (match-string 2 matched))
				   (setq matched
					 (replace-match "" nil nil matched 1)))
				 (expand-file-name (org-strip-quotes matched)
						   dir)))
			   (setq value (replace-match "" nil nil value)))))
		   (only-contents
		    (and (string-match ":only-contents *\\([^: \r\t\n]\\S-*\\)?"
				       value)
			 (prog1 (org-not-nil (match-string 1 value))
			   (setq value (replace-match "" nil nil value)))))
		   (lines
		    (and (string-match
			  ":lines +\"\\([0-9]*-[0-9]*\\)\""
			  value)
			 (prog1 (match-string 1 value)
			   (setq value (replace-match "" nil nil value)))))
		   (env (cond
			 ((string-match "\\<example\\>" value) 'literal)
			 ((string-match "\\<export\\(?: +\\(.*\\)\\)?" value)
			  'literal)
			 ((string-match "\\<src\\(?: +\\(.*\\)\\)?" value)
			  'literal)))
		   ;; Minimal level of included file defaults to the
		   ;; child level of the current headline, if any, or
		   ;; one.  It only applies is the file is meant to be
		   ;; included as an Org one.
		   (minlevel
		    (and (not env)
			 (if (string-match ":minlevel +\\([0-9]+\\)" value)
			     (prog1 (string-to-number (match-string 1 value))
			       (setq value (replace-match "" nil nil value)))
			   (get-text-property (point)
					      :org-include-induced-level))))
		   (args (and (eq env 'literal) (match-string 1 value)))
		   (block (and (string-match "\\<\\(\\S-+\\)\\>" value)
			       (match-string 1 value))))
	      ;; Remove keyword.
	      (delete-region (point) (line-beginning-position 2))
	      (cond
	       ((not file) nil)
	       ((not (file-readable-p file))
		(error "Cannot include file %s" file))
	       ;; Check if files has already been parsed.  Look after
	       ;; inclusion lines too, as different parts of the same
	       ;; file can be included too.
	       ((member (list file lines) included)
		(error "Recursive file inclusion: %s" file))
	       (t
		(cond
		 ((eq env 'literal)
		  (insert
		   (let ((ind-str (make-string ind ?\s))
			 (arg-str (if (stringp args) (format " %s" args) ""))
			 (contents
			  (org-escape-code-in-string
			   (org-export--prepare-file-contents file lines))))
		     (format "%s#+BEGIN_%s%s\n%s%s#+END_%s\n"
			     ind-str block arg-str contents ind-str block))))
		 ((stringp block)
		  (insert
		   (let ((ind-str (make-string ind ?\s))
			 (contents
			  (org-export--prepare-file-contents file lines)))
		     (format "%s#+BEGIN_%s\n%s%s#+END_%s\n"
			     ind-str block contents ind-str block))))
		 (t
		  (insert
		   (with-temp-buffer
		     (let ((org-inhibit-startup t)
			   (lines
			    (if location
				(org-export--inclusion-absolute-lines
				 file location only-contents lines)
			      lines)))
		       (org-mode)
		       (insert
			(org-export--prepare-file-contents
			 file lines ind minlevel
			 (or (gethash file file-prefix)
			     (puthash file
				      (cl-incf current-prefix)
				      file-prefix))
			 footnotes
			 includer-file)))
		     (org-export-expand-include-keyword
		      (cons (list file lines) included)
		      (file-name-directory file)
		      footnotes)
		     (buffer-string)))))
		;; Expand footnotes after all files have been
		;; included.  Footnotes are stored at end of buffer.
		(unless included
		  (org-with-wide-buffer
		   (goto-char (point-max))
		   (maphash (lambda (k v)
			      (insert (format "\n[fn:%s] %s\n" k v)))
			    footnotes))))))))))))

(defun ip-preview ()
  "Preview current include block contents."
  (interactive)

  )
(defun ip-update-cache ()
  "Update include block cache."
  )

(defun ip-get-option ()
  "Get the current include block eval options."
  (let ((include-re "^[ \t]*#\\+INCLUDE:"))
    )
  )

(defun ip-eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

(defun org-in-commented-heading-p (&optional no-inheritance)
  "Non-nil if point is under a commented heading.
This function also checks ancestors of the current headline,
unless optional argument NO-INHERITANCE is non-nil."
  (cond
   ((org-before-first-heading-p) nil)
   ((let ((headline (nth 4 (org-heading-components))))
      (and headline
	   (let ((case-fold-search nil))
	     (string-match-p (concat "^" org-comment-string "\\(?: \\|$\\)")
			     headline)))))
   (no-inheritance nil)
   (t
    (save-excursion (and (org-up-heading-safe) (org-in-commented-heading-p))))))

(defun ip-is-plus-p ()
"Non-nil if include block with plus eval. "
(save-excursion
  (beginning-of-line 0)
  (looking-at-p ip-plus-re))
  )

(defun ip-get-eval ()
  "Return include plus block functions. "
  (save-excursion
    (let* ((funcs ""))
      (while (and (not (eq (point) (point-min)))
                  (not (looking-at-p "^[ \t]*$")))
        (beginning-of-line 0)
        (when (looking-at-p ip-plus-re)
          (let* ((element (org-element-at-point))
                 (value (org-element-property :value element)))
            (setq funcs (concat value funcs )))))
      funcs)
    ))
(provide 'includeplus)
;;; includeplus.el ends here
