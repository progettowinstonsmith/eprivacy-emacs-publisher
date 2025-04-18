;;; edoc-export.el --- Funzioni di esportazione -*- lexical-binding: t -*-

(require 'edoc-org-program)
(defun edoc--org-product-paths (file)
  "Restituisce la lista completa dei path dei file da produrre (in `public/`) per un FILE Org."
  (let* ((base (file-name-base file))
         (relpath (file-relative-name edoc-current-edition-path
                                      (edoc--repo-path "private")))
         (target-dir (expand-file-name relpath (edoc--repo-path "public")))
         (product-line (edoc--read-org-keyword-value file "PRODUCT")))
    (if product-line
        ;; split su virgole e/o spazi, rimuove vuoti
        (let ((names (split-string product-line "[,[:space:]]+" t)))
          (mapcar (lambda (name) (expand-file-name name target-dir)) names))
      ;; default: nome file org -> .md
      (list (expand-file-name (concat base ".md") target-dir)))))

(defun edoc--product-exists-p (file)
  "Restituisce t se almeno uno dei file prodotti da FILE esiste già."
  (seq-some #'file-exists-p (edoc--org-product-paths file)))


(defun edoc--read-org-keyword-value (file key)
  "Restituisce il valore della keyword `#+KEY:` nel FILE, oppure nil se assente."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 1000)
      (org-mode)
      (goto-char (point-min))
      (when (re-search-forward (concat "^#\\+" (upcase key) ":\\s-*\\(.*\\)$") nil t)
        (string-trim (match-string 1))))))


(defun edoc--prepend-md-header (md-file header)
  "Aggiunge HEADER (stringa multilinea) in testa a MD-FILE."
  (when (and (file-exists-p md-file)
             (file-writable-p md-file))
    (let ((old-content (with-temp-buffer
                         (insert-file-contents md-file)
                         (buffer-string))))
      (with-temp-file md-file
        (insert header)
        (insert old-content)))))

(defun edoc-dashboard-publish-file ()
  "Esporta il file Org corrente in Markdown nella directory public, e lo blocca.
Se esiste `#+PWS_EXPORT:` viene chiamata una funzione personalizzata `edoc-export-org-<label>-to-markdown`.
Se esiste `#+PRODUCT:` vengono generati più file. Se almeno uno esiste, viene chiesta conferma per sovrascrivere.
Chiede conferma anche per il blocco del file al termine."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
        (line (line-number-at-pos)))
    (if (not file)
        (message "⚠ Nessun file selezionato.")
      (if (edoc--get-org-lock file)
          (message "❌ Il file è bloccato. Usa `X` per sbloccarlo.")
        (let* ((product-paths (edoc--org-product-paths file))
               (export-label (edoc--read-org-keyword-value file "PWS_EXPORT")))
          ;; Conferma se esistono già prodotti
          (when (and product-paths
                     (edoc--product-exists-p file)
                     (not (yes-or-no-p "⚠ Alcuni file di destinazione esistono. Sovrascriverli?")))
            (user-error "⛔ Esportazione annullata."))
          ;; Crea directory
          (dolist (p product-paths)
            (let ((dir (file-name-directory p)))
              (unless (file-directory-p dir)
                (make-directory dir t))))
          ;; Esegue l'esportazione
          (cond
           ;; Personalizzata via PWS_EXPORT
           (export-label
            (let* ((fun-name (intern (format "edoc-export-org-%s-to-markdown" export-label))))
              (if (fboundp fun-name)
                  (progn
                    (funcall fun-name file (car product-paths))
                    (when (yes-or-no-p "🔒 Bloccare il file dopo l'esportazione?")
                      (edoc--set-lock file t))
                    (message "✅ Esportato con `%s`:\n→ %s"
                             export-label
                             (mapconcat #'file-relative-name product-paths "\n→ "))
                    (edoc-dashboard-refresh)
                    (edoc--goto-line line))
                (message "❌ Funzione export '%s' non trovata." fun-name))))
  	   (t
  	    (let* ((vars-path (expand-file-name "vars.org" edoc-current-edition-path))
		   (options-path (expand-file-name "options.org" edoc-current-edition-path))
  		   (target-md (car product-paths))
  		   (vars (edoc--setup-synth-vars
  			  (edoc--combine-org-vars file)
  			  edoc--synth-rules-vars
  			  file))
  		   (header (edoc--org-vars-to-md-header vars))
  		   (org-source (with-temp-buffer
				 (when (file-exists-p options-path)
				   (insert-file-contents options-path))
  				 (insert-file-contents file)
  				 (buffer-string)))
  		   (exported-md (org-export-string-as org-source 'md t)))
  	      ;; Scrive il file finale con intestazione + corpo
  	      (with-temp-file target-md
  		(insert header)
  		(insert exported-md))
	      
  	      ;; Genera il file .md5
  	      (edoc--write-md5-file product-paths)
	      
  	      ;; Blocca se confermato
  	      (when (yes-or-no-p "🔒 Bloccare il file dopo l'esportazione?")
  		(edoc--set-lock file t))
	      
  	      ;; Mostra conferma e aggiorna UI
  	      (message "✅ Esportato:\n→ %s"
  		       (mapconcat #'file-relative-name product-paths "\n→ "))
  	      (edoc-dashboard-refresh)
  	      (edoc--goto-line line))
  	    )))))))


(defun edoc--setup-synth-vars (vars rules file)
  "Ritorna un alist con VARS esteso dalle variabili sintetiche definite in RULES.
Ogni funzione di RULES riceve l'alist completo `result`, che include anche:
 - FILE: il path assoluto del file
 - FILE_BASE: il nome del file senza estensione"
  (let* ((file (expand-file-name file))
         (file-base (file-name-base file))
         (result (copy-sequence vars)))
    ;; Iniettiamo le info di contesto nel set di variabili
    ;; Applichiamo le regole sintetiche in ordine
    (dolist (rule rules result)
      (let ((key (car rule))
            (synth-fn (cdr rule)))
        (unless (assoc key result)
          (let ((value (funcall synth-fn result file)))
            (setq result (plist-put result  key value))))))))


(defun edoc--read-org-keywords (file)
  "Legge tutte le keyword `#+KEY:` da FILE e restituisce un plist."
  (with-temp-buffer
    (insert-file-contents file nil 0 1000)
    (org-mode)
    (let ((parsed (org-element-parse-buffer))
          result)
      (org-element-map parsed 'keyword
        (lambda (el)
          (let ((key (org-element-property :key el))
                (val (org-element-property :value el)))
            (when (and key val)
              (setq result (plist-put result
                                      (intern (concat ":" (downcase key)))
                                      (string-trim val)))))))
      result)))

(defun edoc--combine-org-vars (file)
  "Combina le variabili di vars.org e quelle locali del FILE.
Le keyword locali sovrascrivono quelle globali. LOCK viene escluso."
  (let* ((vars-file (expand-file-name "vars.org" edoc-current-edition-path))
	 (global-vars (edoc-parse-vars vars-file))
         (local-vars (edoc--read-org-keywords file))
         (combined (copy-sequence global-vars)))
    ;; Sovrascrivi le globali con le locali
    (while local-vars
      (let ((key (pop local-vars))
            (val (pop local-vars)))
        (setq combined (plist-put combined key val))))
    ;; Rimuovi LOCK
    (setq combined (edoc--remove-from-plist combined :lock))
    combined))

(defun edoc--remove-from-plist (plist key)
  "Rimuove la coppia KEY dal PLIST se presente."
  (let (result)
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (eq k key)
          (setq result (plist-put result k v)))))
    result))

(defun edoc-dashboard-publish-all ()
  "Esporta tutti i file Org abilitati dell’edizione corrente."
  (interactive)
  (let* ((dir edoc-current-edition-path)
         (files (directory-files dir t "\\.org$"))
         (enabled-files (seq-filter #'edoc--get-org-enabled files)))
    (if (null enabled-files)
        (message "⚠ Nessun file abilitato da esportare.")
      (when (yes-or-no-p (format "Esportare %d file abilitati?" (length enabled-files)))
        (dolist (file enabled-files)
          (with-temp-buffer
            (insert-file-contents file nil 0 1000)
            (goto-char (point-min))
            (let ((buf (current-buffer)))
              (let ((inhibit-message t)
                    (edoc--in-batch-export t)) ;; variabile dinamica per silenziare messaggi
                (let ((buffer (current-buffer)))
                  (with-current-buffer (get-buffer-create "*edoc-export-log*")
                    (goto-char (point-max))
                    (insert (format "\n---\nExporting: %s\n" (file-name-nondirectory file))))
                  ;; imposta proprietà temporanee e chiama la funzione
                  (with-current-buffer (get-buffer "*PWS Dashboard*")
                    (save-excursion
                      (goto-char (point-min))
                      (when (re-search-forward (regexp-quote (file-name-nondirectory file)) nil t)
                        (put-text-property (point-at-bol) (point-at-eol) 'edoc-file file)
                        (goto-char (point-at-bol))
                        (call-interactively #'edoc-dashboard-publish-file)))))))))
        (message "✅ Esportazione batch completata.")
        (display-buffer "*edoc-export-log*")))))

(defun edoc-dashboard-upload-public ()
  "Esegue `make rsync_upload` nella directory public del sito e aggiorna `.last`."
  (interactive)
  (let ((public-root (edoc--repo-path "public")))
    (if (file-directory-p public-root)
        (edoc-upload-if-ok public-root)
      (message "⚠️  Directory public non trovata: %s" public-root))))

(defun edoc-upload-if-ok (public-root)
  "Esegue `make rsync_upload` nella directory PUBLIC-ROOT. Se il comando ha successo,
crea o aggiorna il file `.last` nella stessa directory con il timestamp corrente."
  (interactive "DDirectory principale della public/: ")
  (let ((default-directory (file-name-as-directory (expand-file-name public-root)))
	(last-file ".last"))
    (message "Avvio: make rsync_upload in %s" default-directory)
    (let ((exit-code (call-process "make" nil "*rsync-upload*" t "rsync_upload")))
      (if (eq exit-code 0)
          (progn
            (with-temp-file (expand-file-name last-file default-directory)
              (insert (format-time-string "%Y-%m-%d %H:%M:%S\n")))
            (message "Upload completato con successo. File `.last` aggiornato."))
	(message "Errore durante l'upload (codice %s). File `.last` NON aggiornato." exit-code)))))

(provide 'edoc-export)
