;;; edoc-import.el --- Importazione da Markdown a Org -*- lexical-binding: t -*-

(require 'edoc) ;; usa edoc--repo-path e edoc-current-edition-path

(defun edoc-import-markdown ()
  "Importa un file Markdown dal repo pubblico a quello privato come file Org, se non esiste già."
  (interactive)
  (let* ((public-root (edoc--repo-path "public"))
         (private-root (edoc--repo-path "private"))
         ;; percorso relativo dell’edizione corrente (es. content/2025/summer)
         (relative-path (file-relative-name edoc-current-edition-path private-root))
         (public-dir (expand-file-name relative-path public-root))
         (private-dir (expand-file-name relative-path private-root))
         (file (read-file-name "Importa markdown: " public-dir nil t nil
                               (lambda (f) (string-match "\\.md$" f))))
         (basename (file-name-base file))
         (org-file (expand-file-name (concat basename ".org") private-dir)))
    (if (file-exists-p org-file)
        (message "⚠️ Il file %s esiste già nel repository privato." (file-name-nondirectory org-file))
      (unless (file-directory-p private-dir)
        (make-directory private-dir t))
      (let* ((clean-md (make-temp-file "edoc-md-clean" nil ".md")))
  	;; pulizia iniziale: elimina chiavi tipo YAML
  	(with-temp-buffer
  	  (insert-file-contents file)
  	  (goto-char (point-min))
  	  ;; Rimuove le righe iniziali tipo chiave: valore fino alla prima riga vuota
  	  (while (and (not (eobp))
  		      (looking-at-p "[^[:space:]]+:"))
  	    (kill-whole-line))
  	  ;; Se la riga successiva è vuota, toglila pure
  	  (when (looking-at-p "^\\s-*$")
  	    (kill-whole-line))
  	  ;; Scrivi su file temporaneo
  	  (write-region (point-min) (point-max) clean-md))
  	;; Ora lancia pandoc sul file temporaneo pulito

  	;; Aspetta fino a 30 secondi che il file sia leggibile
  	(let ((waited 0))
  	  (while (and (not (file-readable-p clean-md))
  		      (< waited 30))
  	    (sleep-for 0.1)
  	    (setq waited (+ waited 0.1))))

  	(if (not (file-readable-p clean-md))
  	    (message "❌ File temporaneo non pronto dopo 30 secondi.")
  	  (let ((cmd (format "pandoc -f markdown -t org %s -o %s"
  			     (shell-quote-argument clean-md)
  			     (shell-quote-argument (expand-file-name org-file)))))
  	    (if (= (shell-command cmd) 0)
  		(progn
  		  (message "✅ Importato e convertito: %s → %s"
  			   (file-name-nondirectory file)
  			   (file-name-nondirectory org-file))
  		  (sit-for 0.5)
  		  (edoc-dashboard-refresh))
  	      (message "❌ Errore durante la conversione con pandoc."))))))))

(provide 'edoc-import)

(defun edoc--write-md5-file (paths)
  "Scrive l'MD5 della concatenazione dei contenuti dei file in PATHS in un file nascosto `.nome.md5`.
    Usa il primo file per determinare il nome dell'hash file."
  (when paths
    (let* ((combined
            (mapconcat (lambda (p)
                         (when (file-readable-p p)
                           (with-temp-buffer
                             (insert-file-contents p)
                             (buffer-string))))
                       paths
                       ""))
           (md5-hash (md5 combined))
           (hash-file (concat (file-name-directory (car paths))
                              "." (file-name-nondirectory (car paths))
                              ".md5")))
      (with-temp-file hash-file
        (insert md5-hash))
      hash-file)))

(defun edoc--md5-up-to-date-p (paths)
  "Restituisce t se il contenuto MD5 dei PATHS corrisponde al valore nel file `.nome.md5`."
  (when paths
    (let* ((first-file (car paths))
           (hash-file (concat (file-name-directory first-file)
                              "." (file-name-nondirectory first-file)
                              ".md5")))
      (when (and (file-exists-p hash-file)
                 (file-readable-p hash-file))
        (let* ((actual-md5 (md5 (mapconcat
                                 (lambda (p)
                                   (with-temp-buffer
                                     (insert-file-contents p)
                                     (buffer-string)))
                                 paths "")))
               (stored-md5 (with-temp-buffer
                             (insert-file-contents hash-file)
                             (string-trim (buffer-string)))))
          (string= actual-md5 stored-md5))))))

(defun edoc--org-product-paths (file)
  "Restituisce la lista completa dei path dei file Markdown da produrre per FILE Org.

  Se il file contiene la keyword `#+PRODUCT:`, i nomi elencati vengono usati.
  Altrimenti viene usato il nome base del file con estensione `.md`, nella directory public."
  (let* ((base (file-name-base file))
         (relpath (file-relative-name edoc-current-edition-path
                                      (edoc--repo-path "private")))
         (target-dir (expand-file-name relpath (edoc--repo-path "public")))
         (product-line (edoc--read-org-keyword-value file "PRODUCT")))
    (if product-line
        ;; Split su virgole e/o spazi, rimuove stringhe vuote
        (let ((names (split-string product-line "[,[:space:]]+" t)))
          (mapcar (lambda (name)
                    (expand-file-name name target-dir))
                  names))
      ;; Default: nome del file .org → .md
      (list (expand-file-name (concat base ".md") target-dir)))))

(defun edoc--read-org-keyword-value (file key)
  "Restituisce il valore della keyword `#+KEY:` nel FILE, oppure nil se assente."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 1000)
      (org-mode)
      (goto-char (point-min))
      (when (re-search-forward (concat "^#\\+" (upcase key) ":\\s-*\\(.*\\)$") nil t)
        (string-trim (match-string 1))))))
