;;; includeplus.el -*- lexical-binding: t; -*-
;;; Code:
;;;
;; (require 'includeplusplus)

(defvar ip-plus-re "^[ \t]*#\\+plus:"
  "Include plus block main symbol regexp.")

(defvar ip-plus-vars nil
  "Include plus variable for transfer in multi functions.")

(defvar ip-relative-path-re-list '("\\[\\[file:\\(.+?\\)]\\(?:\\[\\(\\(?:\\|.\\)+?\\)]\\)?]")
  "Include plus update relative path regexp list.")

(defun ip-update-include-plus (orig-fn &optional included dir footnotes)
  "update every include keyword plus in buffer.
Optional argument INCLUDED is a list of included file names along
with their line restriction, when appropriate.  It is used to
avoid infinite recursion.  Optional argument DIR is the current
working directory.  It is used to properly resolve relative
paths.  Optional argument FOOTNOTES is a hash-table used for
storing and resolving footnotes.  It is created automatically."
  (let ((includer-file (buffer-file-name (buffer-base-buffer)))
        (case-fold-search t)
        ip-cache-list
        (include-re "^[ \t]*#\\+INCLUDE:"))
    ;; Expand INCLUDE keywords.
    (goto-char (point-min))
    (while (re-search-forward include-re nil t)
      (when (and (not (org-in-commented-heading-p))
                 (ip-is-plus-p))
        (let ((element (save-match-data (org-element-at-point)))
              (plus (save-match-data (ip-get-eval))))
          (when (eq (org-element-type element) 'keyword)
            (beginning-of-line)
            ;; Extract arguments from keyword's value.
            (let* ((value (org-element-property :value element))
                   location
                   (file
                    (and (string-match "^\\(\".*?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)"
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
                           )))
                   (plus-file (ip-create-plus-file plus file footnotes))
                   )

              ;; update plus file
              (when plus-file
                (push plus-file ip-cache-list)
                (if (looking-at  "^[ \t]*#\\+INCLUDE: *\\(\".*?\"\\|\\S-+\\)\\(?:\\s-+\\|$\\)")
                    (replace-match (concat " \"" plus-file (if location (concat "::" location) "") "\"") :fixedcase :literal nil 1))
                )
              ;; TODO: remove plus keywords

              )))))
    ;; expand normal include keyword
    (funcall orig-fn included dir footnotes)

    ;; remove create temp cached files
    ;;
    (mapc #'delete-file ip-cache-list)
    ))

(defun ip-preview ()
  "Preview current include block contents."
  (interactive)

  )
(defun ip-update-cache ()
  "Update include block cache."
  )

(defun ip-eval-string (string)
  (eval (car (read-from-string (format "(progn %s)" string)))))

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

(defun ip-create-plus-file (plus &optional file footnotes)
  "Create plus cache file in current dir or same dir with file."
  (let* ((plus-file (if file
                        (if (file-directory-p file)
                            (ip-make-temp-file ".ip-cache-" file)
                          (ip-make-temp-file ".ip-cache-" (file-name-directory file)))
                      (ip-make-temp-file ".ip-cache-"))))
    (with-temp-file plus-file
      ;; insert src file contents
      (cond
       ((or (not file) (file-directory-p file)) nil)
       ((not (file-readable-p file))
        (error "Cannot read src file %s" file))
       (t
        (insert (with-temp-buffer
                  (let ((org-inhibit-startup t))
                    (org-mode)
                    (insert
                     (org-export--prepare-file-contents
                      file
                      )))
                  ;; TODO: consider self include question
                  ;; TODO: confirm relative path embed include question
                  (ip-update-relative-path)
                  (org-export-expand-include-keyword
                   nil
                   (file-name-directory file)
                   footnotes
                   )
                  (buffer-string)))
        ))

      (if (stringp plus)
          (ip-eval-string plus))

      plus-file))
  )

(defun ip-make-temp-file (prefix &optional in-dir dir-flag suffix text)
  "Create a include plus temporary file."
  (let ((absolute-prefix
         (if in-dir
             (expand-file-name prefix in-dir)
           (expand-file-name prefix "./"))))
    (if (find-file-name-handler absolute-prefix 'write-region)
        (files--make-magic-temp-file absolute-prefix dir-flag suffix text)
      (make-temp-file-internal absolute-prefix
                               (if dir-flag t) (or suffix "") text))))

(defun ip-update-relative-path (&optional dir)
  "Update relative path to DIR.

 when file in string-mathch data in ip-relatvie-path-re-list."
  (when ip-relative-path-re-list
    (mapc
     (lambda(re)
       (save-excursion
         (goto-char (point-min))
         (while
             (re-search-forward re nil t)
           (let* ((file (match-string-no-properties 1))
                  (r-file (expand-file-name file dir)))
             (replace-match r-file :fixedcase :literal nil 1)
             )
           )))
     ip-relative-path-re-list))
  )

(defun ip-get (name &optional default)
  "Get include plus variable."

  )

(advice-add 'org-export-expand-include-keyword :around #'ip-update-include-plus)

(provide 'includeplus)
;;; includeplus.el ends here
