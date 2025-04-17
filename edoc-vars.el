;;; edoc-vars.el --- Parsing del file vars.org -*- lexical-binding: t -*-

(require 'org-element)

(setq edoc--synth-rules-vars
      '((:SLUG . (lambda (vars file)
                   (let ((n (plist-get vars :eprivacy_n))
                         (base (file-name-base file)))
                     (format "e-privacy-%s-%s" (or n "ERROR") base))))))


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

(defun edoc--org-vars-to-md-header (vars)
  "Trasforma un plist di variabili in intestazione Markdown `Chiave: Valore`."
  (let* ((alist (edoc--plist-to-alist vars))
         (lines
          (mapcar (lambda (pair)
                    (format "%s: %s"
                            (capitalize (substring (symbol-name (car pair)) 1))
                            (cdr pair)))
                  (sort alist (lambda (a b)
                                (string< (symbol-name (car a)) (symbol-name (car b))))))))
    (concat (string-join lines "\n") "\n\n")))


(defun edoc--read-options-line ()
  "Restituisce tutte le opzioni `#+OPTIONS:` da `options.org` come stringa unica."
  (let* ((path (expand-file-name "options.org" edoc-current-edition-path))
         (lines (when (file-readable-p path)
                  (with-temp-buffer
                    (insert-file-contents path)
                    (goto-char (point-min))
                    (let (results)
                      (while (re-search-forward "^#\\+OPTIONS:\\s-*\\(.*\\)" nil t)
                        (push (match-string 1) results))
                      (string-join (nreverse results) " "))))))
    (or lines "—")))


(provide 'edoc-vars)
