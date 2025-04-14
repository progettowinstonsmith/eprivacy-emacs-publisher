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
