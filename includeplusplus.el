;;; includeplusplus.el -*- lexical-binding: t; -*-
;; TODO:考虑下载作为附件保存情况
(defun ipp-download (url &optional file suffix)
  "Download file for URL, return the download filename."
  (let ((ipp-file (if file (expand-file-name file)  (make-temp-file "ip-" nil suffix))))
    (url-copy-file url ipp-file t t)
    ipp-file)
  )

(defun ipp-get-url (url)
  "Return content for URL as string.
This uses `url-retrieve-synchronously' to make a request with the
URL, then returns the response body.  Since that function returns
the entire response, including headers, we must remove the
headers ourselves."
  (let* ((response-buffer (url-retrieve-synchronously url nil t))
         (encoded-html (with-current-buffer response-buffer
                         ;; Skip HTTP headers.
                         ;; FIXME: Byte-compiling says that `url-http-end-of-headers' is a free
                         ;; variable, which seems to be because it's not declared by url.el with
                         ;; `defvar'.  Yet this seems to work fine...
                         (delete-region (point-min) url-http-end-of-headers)
                         (buffer-string))))
    ;; NOTE: Be careful to kill the buffer, because `url' doesn't close it automatically.
    (kill-buffer response-buffer)
    (with-temp-buffer
      ;; For some reason, running `decode-coding-region' in the
      ;; response buffer has no effect, so we have to do it in a
      ;; temp buffer.
      (insert encoded-html)
      (condition-case nil
          ;; Fix undecoded text
          (decode-coding-region (point-min) (point-max) 'utf-8)
        (coding-system-error nil))
      (buffer-string))))

(defun ipp-from-shell (cmd &optional exp)
  "Run shell command then inport stdout."
  (let* ((data (shell-command-to-string cmd))
         (text (with-temp-buffer
                 (insert data)
                 (goto-char (point-min))
                 (if exp (ip-eval-string exp))
                 (buffer-string))))

    (insert text)
    ))

(defun ipp-from-excel (file &optional sheet no-title)
  "Convert excel xls file to csv for import to org mode."
  (let* ((cmd (concat "xlsx2csv " file))
         (data (shell-command-to-string cmd))
         (table (with-temp-buffer
                  (insert data)
                  (org-table-convert-region (point-min) (point-max))
                  (unless no-title
                    (org-table-insert-hline))
                  (buffer-string))))

    (insert table)
    ))


(defun ipp-from-word (file)
  "Convert word docx file to plain text."
  (let* ((cmd (concat "docx2txt " file))
         (data (shell-command-to-string cmd))
         (text (with-temp-buffer
                 (insert data)
                 (buffer-string))))

    (insert text)
    ;; TODO: change list num to star
    ))

(cl-defun ipp-sparse-tree (match &optional todo-only
                                 (match-body t) parents-body)
  "Create a sparse tree that only shows matched headings and parents.
For TODO-ONLY and MATCH see `org-match-sparse-tree'.
If MATCH-BODY is non-nil the bodies of the matches are shown.
If PARENTS-BODY is non-nil the bodies of the parents are shown."
  (org-export-with-buffer-copy
   ;; Create the sparse tree.
   (let ((org-highlight-sparse-tree-matches t))
     (org-match-sparse-tree todo-only match))
   (let ((pt-first (save-excursion
                     (goto-char (point-min))
                     (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
                     (line-beginning-position)))
         (hls org-occur-highlights))
     ;; Hide everything.
     (outline-flag-region pt-first (point-max) t)
     ;; For each occur highlight overlay (the matches).
     (dolist (hl hls)
       (save-excursion
         (goto-char (overlay-start hl))
         ;; Unhide match.
         (outline-show-heading)
         (when match-body (outline-show-entry))
         ;; Unhide parents.
         (while (org-up-heading-safe)
           (outline-show-heading)
           (when parents-body (outline-show-entry))))))
   ;; Hide all archived subtrees again.
   (org-hide-archived-subtrees (point-min) (point-max))
   (org-copy-visible (point-min) (point-max))
   ;; (kill-this-buffer)
   )

  (goto-char (point-min))
  (re-search-forward (concat "^\\(" org-outline-regexp "\\)") nil t)
  (delete-region (line-beginning-position) (point-max))
  (yank)
  )

(provide 'includeplusplus)
