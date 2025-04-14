;;; edoc-dashboard.el --- Interfaccia testuale PWS -*- lexical-binding: t -*-

        (require 'edoc-vars)
        (require 'edoc-git)
        (require 'edoc-import)
        (require 'edoc)

      (defun edoc--org-file-status (filepath)
      "Restituisce il valore della proprietà #+STATUS: in FILEPATH, se esiste."
      (when (file-readable-p filepath)
        (with-temp-buffer        
          (insert-file-contents filepath nil 0 1000) ; solo le prime linee
          (org-mode)
          (goto-char (point-min))
          (if (re-search-forward "^#\\+STATUS:\\s-*\\(.*\\)$" nil t)
              (let ((val (string-trim (match-string 1))))
                (cond
                 ((string-match-p "enabled" val) "✔ enabled")
                 ((string-match-p "draft" val)   "✎ draft")
                 ((string-match-p "disabled" val) "– disattivato")
                 (t val)))
            "– (nessun status)"))))

        (defun edoc--plist-to-alist (plist)
        "Converte un plist in una alist."
        (let (alist)
          (while plist
            (let ((key (car plist))
                  (val (cadr plist)))
              (setq alist (cons (cons key val) alist))
              (setq plist (cddr plist))))
          (nreverse alist)))

      (defun edoc-dashboard-refresh ()
        "Mostra o aggiorna la dashboard."
        (interactive)
        (let* ((edition-path edoc-current-edition-path)
               (vars-path (expand-file-name "vars.org" edition-path))
               (data (edoc-parse-vars vars-path))
               (title (plist-get data :title))
               (num (or (plist-get data :num)
                        (getenv "EPRIVACY_N")))
               (fase (or (plist-get data :fase) "setup"))
               (buf (get-buffer-create "*PWS Dashboard*")))

          (with-current-buffer buf
            (read-only-mode -1)
            (erase-buffer)
            (insert (propertize "PWS EPRIVACY PUBLISHING SYSTEM\n\n" 'face '(:height 1.5 :weight bold)))

            ;; Repo info
            (insert (edoc--dashboard-repo-info "db" (edoc--repo-path "private")))
            (insert (edoc--dashboard-repo-info "site" (edoc--repo-path "public")))

            ;; Edizione corrente
            (insert (format "\nIn lavorazione su: E-PRIVACY %s\n" (or num "??")))
            (insert (format "Fase: %s\n\n" fase))

            ;; Documenti presenti
            (insert (propertize "Documenti presenti nella directory:\n" 'face '(:weight bold)))
            (let* ((doc-files (directory-files edition-path t "\\.org$"))
                   (sorted-docs (sort doc-files #'string<)))
      	(dolist (file sorted-docs)
                (let* ((name (file-name-nondirectory file))
      		 (status (edoc--org-file-status file)))
      	    (let ((start (point)))
      	      (insert (format "• %-15s %s\n" name status))
      	      (add-text-properties
      	       start (point)
      	       `(edoc-file ,file
      			   mouse-face highlight
      			   help-echo "Premi RET per aprire questo file"))))))



            ;; Variabili in ordine alfabetico (allineate)
            (insert (propertize "\nVariabili definite in vars.org:\n" 'face '(:weight bold)))
            (let* ((exclude '(:extra-documents))
                   (vars (edoc--filtered-vars data exclude))
                   (max-key-len (apply #'max (mapcar (lambda (pair)
      						 (length (symbol-name (car pair))))
                                                     vars))))
      	(dolist (pair vars)
                (let* ((key (upcase (substring (symbol-name (car pair)) 1)))
      		 (val (cdr pair)))
                  (insert (format (format "%%-%ds : %%s\n" max-key-len) key val)))))

    	(let ((map (make-sparse-keymap)))
    	  (define-key map (kbd "RET") #'edoc-dashboard-open-file-at-point)
    	  (define-key map (kbd "q") #'quit-window)
    	  (define-key map (kbd "g") #'edoc-dashboard-refresh)
  	  (define-key map (kbd "+") #'edoc-dashboard-enable-file)
  	  (define-key map (kbd "-") #'edoc-dashboard-disable-file)
  	  (define-key map (kbd "d") #'edoc-dashboard-toggle-status)
	  (define-key map (kbd "I") #'edoc-import-markdown)
    	  (use-local-map map))

            (goto-char (point-min))
            (read-only-mode 1)
            (switch-to-buffer buf))))

  (defun edoc-dashboard-toggle-status ()
  "Inverti lo STATUS del file .org sulla riga corrente."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
      (line (line-number-at-pos)))
    (if (not (and file (file-exists-p file)))
        (message "Nessun file .org su questa riga.")
      (let ((current (edoc--get-org-status file)))
        (cond
         ((string= current "enabled")
          (edoc--set-org-status file 'disabled)
          (message "🚫 %s disabilitato." (file-name-nondirectory file)))
         (t
          (edoc--set-org-status file 'enabled)
          (message "✅ %s abilitato." (file-name-nondirectory file))))
        (edoc-dashboard-refresh)                                                                                                                      
	(edoc--goto-line line)))))

  (defun edoc--get-org-status (filepath)
  "Restituisce il valore di `#+STATUS:` in FILEPATH, o nil se non presente."
  (when (file-readable-p filepath)
    (with-temp-buffer
      (insert-file-contents filepath nil 0 1000)
      (org-mode)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+STATUS:\\s-*\\(.*\\)$" nil t)
        (string-trim (match-string 1))))))


    (defun edoc-dashboard-open-file-at-point ()
    "Apre il file .org associato alla riga corrente nella dashboard, se presente."
    (interactive)
    (let ((file (get-text-property (point) 'edoc-file)))
      (if (and file (file-exists-p file))
          (let ((inhibit-read-only t))
    	(switch-to-buffer (find-file-noselect file)))
        (message "Nessun file .org associato a questa riga."))))

      
        (defun edoc--dashboard-repo-info (label repo-path)
          "Restituisce una stringa con lo stato del repository."
          (let* ((commit (edoc-git-last-commit-summary repo-path))
                 (status (cond
                          ((not (file-directory-p repo-path)) "✖ non presente")
                          ((edoc-git-repo-dirty-p repo-path) "⚠ modifiche locali")
                          ((not (edoc-git-up-to-date-p repo-path)) "↯ non aggiornato")
                          (t "✔ aggiornato"))))
            (format "%-4s v. %s\n      %s\n\n" label (or commit "—") status)))

      (defun edoc--filtered-vars (plist &optional exclude)
      "Ritorna una alist ordinata delle variabili in PLIST, escludendo EXCLUDE."
      (let ((exclude (or exclude '(:extra-documents))))
        (sort
         (cl-remove-if (lambda (pair) (member (car pair) exclude))
                       (edoc--plist-to-alist plist))
         (lambda (a b) (string< (symbol-name (car a))
                                (symbol-name (car b)))))))


    (defun edoc--set-org-status (file status)
    "Imposta o aggiorna il valore `#+STATUS:` in FILE con STATUS ('enabled, 'disabled, ecc.)."
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (org-mode)
        (goto-char (point-min))
        (if (re-search-forward "^#\\+STATUS:\\s-*\\(.*\\)$" nil t)
            (replace-match (concat "#+STATUS: " (symbol-name status)))
          ;; Se non c'è, inserisci in alto dopo eventuali #+ altre variabili
          (goto-char (point-min))
          (if (re-search-forward "^#\\+" nil t)
              (progn
                (beginning-of-line)
                (insert "#+STATUS: " (symbol-name status) "\n"))
            (insert "#+STATUS: " (symbol-name status) "\n")))
        (write-region (point-min) (point-max) file))))

    (defun edoc-dashboard-enable-file ()
    "Abilita il file .org nella riga corrente (STATUS → enabled)."
    (interactive)
    (let ((file (get-text-property (point) 'edoc-file))
      (line (line-number-at-pos)))
      (if file
          (progn
            (edoc--set-org-status file 'enabled)
            (message "✅ %s abilitato." (file-name-nondirectory file))
            (edoc-dashboard-refresh)                                                                                                                      
            (edoc--goto-line line))
        (message "Nessun file .org su questa riga."))))

  (defun edoc-dashboard-disable-file ()
    "Disabilita il file .org nella riga corrente (STATUS → disabled)."
    (interactive)
    (let ((file (get-text-property (point) 'edoc-file))
    (line (line-number-at-pos)))
      (if file
          (progn
            (edoc--set-org-status file 'disabled)
            (message "🚫 %s disabilitato." (file-name-nondirectory file))
            (edoc-dashboard-refresh)
	    (edoc--goto-line line))
        (message "Nessun file .org su questa riga."))))

(defun edoc--goto-line (n)
  "Vai alla linea N senza attivare marker."
  (goto-char (point-min))
  (forward-line (1- n)))

        (provide 'edoc-dashboard)
