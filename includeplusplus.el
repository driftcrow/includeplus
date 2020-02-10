;;; includeplusplus.el -*- lexical-binding: t; -*-
(defun ip-download (url &optional file)
"TODO:Download file for URL"
(let ((file (if file (expand-file-name file)  (make-temp-file "ip-"))))
  (url-copy-file url file t t)
  file)
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

(defun ipp-from-excel (file &optional sheet with-title)
  "Convert excel xls file to csv for import to org mode."
  (let* ((cmd (concat "xlsx2csv " file))
         (data (shell-command-to-string cmd))
         (table (with-temp-buffer
                  (insert data)
                  (org-table-convert-region (point-min) (point-max))
                  (if with-title
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

(provide 'includeplusplus)
