;;; edoc-vars.el --- Parsing del file vars.org -*- lexical-binding: t -*-

(require 'org-element)


(defun edoc-parse-vars (file)
"Parsa il file vars.org e restituisce un plist con tutte le keyword #+KEY: e i documenti aggiuntivi."
(with-temp-buffer
  (insert-file-contents file)
  (org-mode)
  (let* ((parsed (org-element-parse-buffer))
         (vars (edoc--extract-all-keywords parsed))
         (extra-docs (edoc--extract-documents-table parsed)))
    (plist-put vars :extra-documents extra-docs)
    vars)))

(defun edoc--extract-all-keywords (parsed)
"Estrae tutte le keyword #+KEY: dal buffer Org come plist, lowercase."
(let (result)
  (org-element-map parsed 'keyword
    (lambda (el)
      (let* ((key (org-element-property :key el))
             (val (string-trim (org-element-property :value el))))
        (setq result (plist-put result (intern (concat ":" (downcase key))) val)))))
  result))

(defun edoc--extract-documents-table (parsed)
  "Estrae la tabella 'Documenti aggiuntivi' dal file e restituisce una lista di alist."
  (let ((tables (org-element-map parsed 'table
                  (lambda (tbl)
                    (let ((title (org-element-property :raw-value
                                                       (org-element-lineage tbl '(headline)))))
                      (when (and title (string-match "Documenti aggiuntivi" title))
                        tbl))))))
    (when tables
      (let* ((table (car tables))
             (rows (org-table-to-lisp
                    (org-element-interpret-data table))))
        ;; Rimuove intestazione
        (let ((header (car rows))
              (body (cdr rows)))
          (mapcar (lambda (row)
                    (cl-pairlis (mapcar #'intern (mapcar #'downcase header)) row))
                  body))))))

(provide 'edoc-vars)
