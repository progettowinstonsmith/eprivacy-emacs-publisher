;; -*- lexical-binding: t; -*-

(require 'edoc-vars)
(require 'edoc-git)
(require 'edoc-import)
(require 'edoc-export)
(require 'edoc-org-program)
(require 'edoc)
(require 'transient)


(defun edoc--repo-on-current-line ()
  "Restituisce il nome logico del repo ('private', 'public' o 'sw') in base alla riga del cursore."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at "^db\\b") "private")
     ((looking-at "^site\\b") "public")
     ((looking-at "^sw\\b") "sw")
     (t nil))))

(defun edoc-dashboard-git-do (operation)
  "Esegue una OPERAZIONE (simbolo: push, pull, commit) sul repo determinato dalla riga corrente."
  (interactive)
  (let* ((repo (edoc--repo-on-current-line))
      	 (repo-path (cond
                     ((string= repo "sw") (expand-file-name "~/epub-system"))
                     (repo (edoc--repo-path repo))
                     (t nil)))
      	 (default-directory repo-path))
    (cond
     ((not repo)
      (message "⚠ Posizionati sulla riga di un repository (db o site)."))
     ((not (file-directory-p repo-path))
      (message "⚠ Il repository '%s' non esiste." repo))
     (t
      (pcase operation
        ('push
         (call-process "git" nil "*edoc-git*" t "push")
         (message "✅ Push completato per %s" repo))
        ('pull
         (call-process "git" nil "*edoc-git*" t "pull")
         (message "✅ Pull completato per %s" repo))
        ('commit-push
         (let ((msg (read-string (format "Messaggio di commit per '%s': " repo))))
           (call-process "git" nil "*edoc-git*" t "add" "-A")
           (call-process "git" nil "*edoc-git*" t "commit" "-m" msg)
           (call-process "git" nil "*edoc-git*" t "push")
           (message "✅ Commit & push completati per %s" repo))))
      (edoc-dashboard-refresh)))))

(defun edoc-dashboard-git-status ()
  "Mostra lo stato del repository corrente (magit o vc-dir) in base alla riga attiva."
  (interactive)
  (let* ((repo (edoc--repo-on-current-line))
      	 (repo-path (cond
                     ((string= repo "sw") (expand-file-name "~/epub-system"))
                     (repo (edoc--repo-path repo))
                     (t nil))))
    (cond
     ((not repo-path)
      (message "⚠ Posizionati su una riga di repo (db, site o sw)."))
     ((not (file-directory-p repo-path))
      (message "⚠ Repository %s non trovato." repo))
     ((fboundp 'magit-status)
      (magit-status repo-path))
     (t
      (vc-dir repo-path)))))


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

(defun edoc-open-public-shell ()
  "Apre una shell interattiva nella directory del repository public."
  (interactive)
  (let ((default-directory (edoc--repo-path "public")))
    (unless (file-directory-p default-directory)
      (user-error "Directory public non trovata: %s" default-directory))
    (shell (generate-new-buffer-name "*Shell: public*"))))

(defvar edoc--first-file-line nil
  "Line of first file in dashboard")

(defun edoc-dashboard-refresh ()
  "Mostra o aggiorna la dashboard."
  (interactive)
  (let* ((edition-path edoc-current-edition-path)
         (vars-path (expand-file-name "vars.org" edition-path))
         (data (edoc-parse-vars vars-path))
         (title (plist-get data :title))
         (num (plist-get data :eprivacy_n))
         (edition (plist-get data :edition))
         (fase (or (plist-get data :fase) "setup"))
         (buf (get-buffer-create "*PWS Dashboard*")))
    
    (with-current-buffer buf
      (read-only-mode -1)
      (erase-buffer)
      (message "Sto leggendo le informazioni relative all'edizione in corso")
      (insert (propertize "PWS E-PRIVACY PS: " 'face '(:height 1.5 :weight bold)))
      (insert " Working on: ")
      (insert (propertize (format "E-PRIVACY %s %s edition" (or num "??") (or edition "??")) 'face '(:height 1.5 :weight bold)))
      (insert (format "[%s] " fase))
      ;; Legge il contenuto di .last nel repo public
      (let* ((public-root (edoc--repo-path "public"))
	     (last-path (expand-file-name ".last" public-root))
	     (last-value (if (file-readable-p last-path)
			     (with-temp-buffer
                               (insert-file-contents last-path)
                               (string-trim (buffer-string)))
			   "void")))
	(insert (format "[%s]" last-value)))
      (when (edoc-devserver-running-p)
	(insert " 🔥 http://test.winstonsmith.org"))
      (insert "\n\n")
      
      ;; Repo info
      (insert (propertize "📝 [Situazione dei repository]\n" 'face '(:weight bold)))
      (insert (edoc--dashboard-repo-info "db" (edoc--repo-path "private")))
      (insert (edoc--dashboard-repo-info "site" (edoc--repo-path "public")))
      (insert (edoc--dashboard-repo-info "sw" (edoc--repo-path "sw")))

      (message "")
      
      
      ;; Documenti presenti
      (insert (propertize "\n📝 [Documenti presenti nella directory:]\n" 'face '(:weight bold)))
      (let* ((doc-files (directory-files edition-path t "\\.org$"))
             (sorted-docs (sort doc-files #'string<)))
        (if (null sorted-docs)
            (progn
              (setq edoc--first-file-line nil)
              (insert "   Nessun documento org trovato\n"))
          (let ((max-name-len (apply #'max (mapcar (lambda (f)
                                         		 (length (file-name-nondirectory f)))
                                         		sorted-docs))))
            (insert (format (format "%%-%ds  🔒  ✅  📝  📄\n\n" max-name-len)
                            (make-string (+ max-name-len 2) ?-)))
            (setq edoc--first-file-line (line-number-at-pos))
            (dolist (file sorted-docs)
              (let* ((name (file-name-nondirectory file))
                     (status (edoc--org-file-status file))
                     (start (point)))
                (let* ((icons (edoc--org-status-icons file)))
                  (insert (format (format "• %%-%ds  %%s  %%s  %%s  %%s\n" max-name-len)
                     		name
                     		(plist-get icons :lock)
                     		(plist-get icons :enabled)
                     		(plist-get icons :draft)
                     		(plist-get icons :md))))
                (add-text-properties
                 start (point)
                 `(edoc-file ,file
                         	 mouse-face highlight
                         	 help-echo "Premi RET per aprire questo file")))))))

      (insert "\n--- ❓ Premi ? per aprire il menu dei comandi e la legenda\n")
      (insert (propertize "\n⚙️  [Opzioni globali Markdown]\n" 'face '(:weight bold)))
      (insert (format "%s\n" (edoc--read-options-line)))

      ;; Variabili in ordine alfabetico (allineate)
      (insert (propertize "\n💡 [Variabili definite in vars.org]\n" 'face '(:weight bold)))
      (let* ((exclude '(:extra-documents))
             (vars (edoc--filtered-vars data exclude)))
        (if (null vars)
            (insert "   Nessuna variabile disponibile\n")
          (let ((max-key-len (apply #'max (mapcar (lambda (pair)
                                        		(length (symbol-name (car pair))))
                                        	    vars))))
            (dolist (pair vars)
              (let* ((key (upcase (substring (symbol-name (car pair)) 1)))
                     (val (cdr pair)))
                (insert (format (format "%%-%ds : %%s\n" max-key-len) key val)))))))
      
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "?") #'edoc-dashboard-menu)
        (define-key map (kbd "RET") #'edoc-dashboard-open-file-at-point)
    	(define-key map (kbd "(") #'edoc-dashboard-open-private-dired)
    	(define-key map (kbd ")") #'edoc-dashboard-open-public-dired)
      	(define-key map (kbd "r p") (lambda () (interactive) (edoc-dashboard-git-do 'push)))
      	(define-key map (kbd "r P") (lambda () (interactive) (edoc-dashboard-git-do 'pull)))
      	(define-key map (kbd "r g") (lambda () (interactive) (edoc-dashboard-git-do 'commit-push)))
      	(define-key map (kbd "r s") #'edoc-dashboard-git-status)
	(define-key map (kbd "=") #'edoc-devserver-toggle)
	(define-key map (kbd "q") #'quit-window)
	(define-key map (kbd "g") #'edoc-dashboard-refresh)
	(define-key map (kbd "S") #'edoc-dashboard-cycle-status)
	(define-key map (kbd "E") #'edoc-dashboard-toggle-enable)
	(define-key map (kbd "D") #'edoc-dashboard-toggle-enable)
	(define-key map (kbd "L") #'edoc-dashboard-toggle-lock)
	(define-key map (kbd "U") #'edoc-dashboard-toggle-lock)
	(define-key map (kbd "I") #'edoc-import-markdown)
	(define-key map (kbd "p") #'edoc-dashboard-publish-file)
	(define-key map (kbd "*") #'edoc-dashboard-upload-public)
  	(define-key map (kbd "!") #'edoc-open-public-shell)
    	(define-key map (kbd "C") #'edoc-dashboard-create-org-file)
      	(define-key map (kbd "M") #'edoc-dashboard-open-md)
	(define-key map (kbd "P") #'edoc-dashboard-publish-all)
	(define-key map (kbd "B") #'edoc-dashboard-sync-markdown)
	(use-local-map map))
      
      (goto-char (point-min))
      (when edoc--first-file-line
	(forward-line (1- edoc--first-file-line)))
      (read-only-mode 1)
      (switch-to-buffer buf))))



(defun edoc-dashboard-create-org-file ()
  "Crea un nuovo file Org nella directory dell’edizione corrente e lo apre per l’editing."
  (interactive)
  (let* ((name (read-string "Nome del nuovo file (senza estensione): "))
      	 (filename (concat name ".org"))
      	 (target-dir edoc-current-edition-path)
      	 (path (expand-file-name filename target-dir)))
    (if (file-exists-p path)
      	(message "⚠ Il file '%s' esiste già." filename)
      (progn
      	(unless (file-directory-p target-dir)
          (make-directory target-dir t))
      	(with-temp-buffer
          (insert (format "#+TITLE: %s\n\n" name))
          (write-file path))
      	(message "✅ File creato: %s" path)
      	(find-file path)))))


(defun edoc--org-file-md-exists-p (org-file)
  "Restituisce t se esiste un file .md corrispondente a ORG-FILE nel repo public."
  (let* ((name (file-name-base org-file))
         (md-path (expand-file-name (concat name ".md")
                                    (expand-file-name
                                     (file-relative-name edoc-current-edition-path
                                                         (edoc--repo-path "private"))
                                     (edoc--repo-path "public")))))
    (file-exists-p md-path)))

(defun edoc--md5-status-icon (file)
  "Restituisce un'icona che indica lo stato del file Markdown esportato."
  (let* ((paths (edoc--org-product-paths file)))
    (cond
     ((not (seq-some #'file-exists-p paths)) "—")            ;; nessun file presente
     ((edoc--md5-up-to-date-p paths) "✔")                    ;; hash corrispondente
     (t "✎"))))                                               ;; esiste ma modificato

(defun edoc-dashboard-unlock-file ()
  "Sblocca un file Org (cambia STATUS da locked a enabled previa conferma)."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
        (line (line-number-at-pos)))
    (when (and file
               (string= (edoc--get-org-status file) "locked")
               (yes-or-no-p (format "Sbloccare il file %s?" (file-name-nondirectory file))))
      (edoc--set-org-status file 'enabled)
      (message "🔓 File sbloccato.")
      (edoc-dashboard-refresh)
      (edoc--goto-line line))))

(defun edoc-dashboard-lock-file ()
  "Blocca il file Org sulla riga corrente (STATUS → locked)."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
        (line (line-number-at-pos)))
    (if (not file)
        (message "Nessun file selezionato.")
      (let ((status (edoc--get-org-status file)))
        (if (string= status "locked")
            (message "🔒 Il file è già bloccato.")
          (edoc--set-org-status file 'locked)
          (message "🔒 File bloccato.")
          (edoc-dashboard-refresh)
          (edoc--goto-line line))))))

(defun edoc-dashboard-toggle-lock ()
  "Attiva o disattiva il blocco del file Org sulla riga corrente (modifica `#+LOCK:`)."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
        (line (line-number-at-pos)))
    (if (not file)
        (message "⚠ Nessun file selezionato.")
      (if (edoc--get-org-lock file)
          (when (yes-or-no-p (format "🔓 Sbloccare il file %s?" (file-name-nondirectory file)))
            (edoc--set-lock file nil)
            (message "🔓 File sbloccato.")
            (edoc-dashboard-refresh)
            (edoc--goto-line line))
        (progn
          (edoc--set-lock file t)
          (message "🔒 File bloccato.")
          (edoc-dashboard-refresh)
          (edoc--goto-line line))))))


(defun edoc--set-lock (file locked)
  "Imposta `#+LOCK:` su yes o no nel FILE Org."
  (edoc--set-org-keyword file "LOCK" (if locked "yes" "no")))

(defun edoc--set-enabled (file enabled)
  "Imposta `#+ENABLED:` su yes o no nel FILE Org."
  (edoc--set-org-keyword file "ENABLED" (if enabled "yes" "no")))

(defun edoc--set-draft (file draft)
  "Imposta `#+STATUS:` su draft o cleared nel FILE Org."
  (edoc--set-org-keyword file "STATUS" (if draft "draft" "published")))


(defun edoc--set-org-keyword (file key value)
  "Imposta o aggiorna la keyword `#+KEY:` nel FILE Org con VALUE.
               KEY deve essere una stringa (es: \"LOCK\", \"ENABLED\"). VALUE deve essere una stringa."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (let* ((upkey (upcase key))
             (re (concat "^#\\+" upkey ":\\s-*.*$")))
        (goto-char (point-min))
        (if (re-search-forward re nil t)
            (replace-match (format "#+%s: %s" upkey value))
          ;; Se la keyword non c'è, aggiungila dopo il blocco iniziale
          (goto-char (point-min))
          (while (looking-at "^#\\+")
            (forward-line 1))
          (insert (format "#+%s: %s\n" upkey value))))
      ;; Assicura che ci sia una riga vuota dopo le keyword
      (goto-char (point-min))
      (while (looking-at "^#\\+")
        (forward-line 1))
      (unless (looking-at "^\\s-*$")
        (insert "\n"))
      (write-region (point-min) (point-max) file nil 'silent))))


(defun edoc-dashboard-toggle-enable ()
  "Inverti lo STATUS del file .org sulla riga corrente."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
        (line (line-number-at-pos)))
    (if (not (and file (file-exists-p file)))
        (message "Nessun file .org su questa riga.")
      (let ((enabled (edoc--get-org-enabled file)))
        (if (edoc--get-org-lock file)
            (message "Il file è bloccato 🔒. Non posso operare.")
          (edoc--set-enabled file (not enabled))
          (if enabled
              (message "🚫 %s disabilitato." (file-name-nondirectory file)))
          (message "✅ %s abilitato." (file-name-nondirectory file)))
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

(defun edoc--read-org-keyword-p (file key &optional test-fn)
  "Restituisce t se il file Org FILE contiene `#+KEY:` il cui valore passa TEST-FN.

           Se TEST-FN è nil, considera vero solo se il valore è \"yes\" o \"t\" (case-insensitive)."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file nil 0 1000)
      (org-mode)
      (goto-char (point-min))
      (when (re-search-forward (concat "^#\\+" (upcase key) ":\\s-*\\(.*\\)$") nil t)
        (let ((val (downcase (string-trim (match-string 1)))))
          (if test-fn
              (funcall test-fn val)
            (or (string= val "yes")
                (string= val "t"))))))))

(defun edoc--get-org-lock (file)
  "Restituisce t se `#+LOCK:` è `yes` o `t`."
  (edoc--read-org-keyword-p file "LOCK"))
(defun edoc--get-org-enabled (file)
  "Restituisce t se `#+ENABLED:` è `yes` o `t`."
  (edoc--read-org-keyword-p file "ENABLED"))

(defun edoc--get-org-draft (file)
  "Restituisce t se `#+STATUS:` è `draft` (case-insensitive)."
  (if (file-exists-p file)
      (if (edoc--read-org-keyword-p file "STATUS"
                                    (lambda (val) (string= val "draft")))
      	  "📝" "📄") "❌"))


(defun edoc--get-org-status-value (file)
  "Restituisce il valore della keyword `#+STATUS:` in FILE, in simbolo (default: 'draft)."
  (let ((val (downcase (or (edoc--read-org-keyword-value file "STATUS") "draft"))))
    (cond
     ((string= val "published") 'published)
     ((string= val "hidden")    'hidden)
     ((string= val "skip")      'skip)
     ((string= val "draft")     'draft)
     (t                         'published)))) ; fallback

(defun edoc--status-icon (file)
  "Restituisce un’icona in base alla keyword `#+STATUS:`."
  (pcase (edoc--get-org-status-value file)
    ('draft     "📝")
    ('published "📄")
    ('hidden    "👁️‍🗨️")
    ('skip      "🚫")
    (_          "❓")))

(defun edoc-dashboard-cycle-status ()
  "Cicla il valore della chiave `#+STATUS:` (draft → published → hidden → skip → draft)."
  (interactive)
  (let ((file (get-text-property (point) 'edoc-file))
        (line (line-number-at-pos)))
    (if (not file)
        (message "⚠ Nessun file selezionato.")
      (let* ((current (edoc--get-org-status-value file))
             (order '(draft published hidden skip))
             (next (or (cadr (member current order)) (car order))))
        (edoc--set-org-keyword file "STATUS" (symbol-name next))
        (message "📄 Stato aggiornato a: %s" (capitalize (symbol-name next)))
        (edoc-dashboard-refresh)
        (edoc--goto-line line)))))

(defun edoc--org-status-icons (file)
  "Restituisce un plist con le icone degli stati per il FILE Org."
  (let* ((lock (if (edoc--get-org-lock file) "🔒" "🟩"))
         (enabled (if (edoc--get-org-enabled file) "✅" "⛔"))
         (status (edoc--status-icon file))
         (md-icon (cond
                   ((not (seq-some #'file-exists-p (edoc--org-product-paths file))) "🔴")
                   ((edoc--md5-up-to-date-p (edoc--org-product-paths file)) "🟢")
                   (t "🟡"))))
    (list :lock lock
          :enabled enabled
          :draft status
          :md md-icon)))

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
    (format "%-4s v. %40s      %s\n" label (or commit "—") status)))

(defun edoc--filtered-vars (plist &optional exclude)
  "Ritorna una alist ordinata delle variabili in PLIST, escludendo EXCLUDE."
  (let ((exclude (or exclude '(:extra-documents))))
    (sort
     (cl-remove-if (lambda (pair) (member (car pair) exclude))
                   (edoc--plist-to-alist plist))
     (lambda (a b) (string< (symbol-name (car a))
                            (symbol-name (car b)))))))


(defun edoc-dashboard-open-private-dired ()
  "Apre dired nella directory di lavoro dell’edizione corrente (repo private)."
  (interactive)
  (let ((path edoc-current-edition-path))
    (if (and path (file-directory-p path))
        (dired path)
      (message "⚠ Nessuna edizione corrente o directory non trovata."))))

(defun edoc-dashboard-open-public-dired ()
  "Apre dired nella directory corrispondente nel repo public."
  (interactive)
  (let* ((relpath (file-relative-name edoc-current-edition-path
                                      (edoc--repo-path "private")))
         (public-path (expand-file-name relpath (edoc--repo-path "public"))))
    (if (file-directory-p public-path)
        (dired public-path)
      (message "⚠ Directory public non trovata: %s" public-path))))

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


(defun edoc--goto-line (n)
  "Vai alla linea N senza attivare marker."
  (goto-char (point-min))
  (forward-line (1- n)))

(defun edoc-dashboard-open-md ()
  "Apre il file Markdown corrispondente al file Org sotto il cursore, se esiste."
  (interactive)
  (let* ((org-file (get-text-property (point) 'edoc-file))
      	 (line (line-number-at-pos)))
    (if (not org-file)
      	(message "⚠ Nessun file selezionato.")
      (let* ((base (file-name-base org-file))
             (relpath (file-relative-name edoc-current-edition-path
                                          (edoc--repo-path "private")))
             (md-file (expand-file-name (concat base ".md")
      					(expand-file-name relpath
                                                          (edoc--repo-path "public")))))
      	(if (file-exists-p md-file)
            (find-file md-file)
          (message "❌ File Markdown non trovato: %s" md-file))))))

(require 'transient)

(transient-define-prefix edoc-dashboard-menu ()
  "Pannello comandi della dashboard EPUB."
  [["🔁 Refresh / Navigazione"
    ("g"  "Aggiorna dashboard" edoc-dashboard-refresh)
    ("RET" "Apri file (org)" edoc-dashboard-open-file-at-point)
    ("M"  "Apri file Markdown" edoc-dashboard-open-md)
    ("("  "Dired private" edoc-dashboard-open-private-dired)
    (")"  "Dired public"  edoc-dashboard-open-public-dired)]

   ["📂 File Org"
    ("C" "Crea nuovo file org" edoc-dashboard-create-org-file)
    ("p" "Pubblica file corrente" edoc-dashboard-publish-file)
    ("P" "Pubblica tutto" edoc-dashboard-publish-all)
    ("B" "Backup + diff Markdown" edoc-dashboard-sync-markdown)
    ("I" "Importa da public" edoc-import-markdown)]

   ["📡 Git"
    ("r s" "Git status" edoc-dashboard-git-status)
    ("r p" "Push"       (lambda () (interactive) (edoc-dashboard-git-do 'push)))
    ("r P" "Pull"       (lambda () (interactive) (edoc-dashboard-git-do 'pull)))
    ("r g" "Commit + Push" (lambda () (interactive) (edoc-dashboard-git-do 'commit-push)))]

   ["🚀 Altri"
    ("*" "Upload sito (rsync)" edoc-dashboard-upload-public)
    ("!" "Apri shell in public/" edoc-open-public-shell)
    ("=" "Attiva/disattiva dev server" edoc-devserver-toggle)
    ("q" "Chiudi dashboard" quit-window)]

   ["📄 Legenda Colonne:"
    ("L" "🔒 / 🟩 = Locked / Unlocked" edoc-dashboard-toggle-lock)
    ("D" "✅ / ⛔ = Enabled / Disabled" edoc-dashboard-toggle-enable)
    ("3" "📝 / 📄 / 👁️‍🗨️ / 🚫 = Draft / Pubblicato / Nascosto / Skip" 
     edoc-dashboard-cycle-status)
    ("-" "🔴 / 🟡 / 🟢 = No MD5 / MD5 errata / MD5 ok"
     (lambda () (interactive) (message "Legenda — stato MD5")))]])

(defvar edoc--devserver-buffer-name "*Shell:Devserver*"
  "Buffer dedicato al devserver pelican.")

(defun edoc-devserver-running-p ()
  "Restituisce t se il devserver è attivo (cioè se esiste un buffer con processo vivo)."
  (let ((proc (get-buffer-process edoc--devserver-buffer-name)))
    (and proc (process-live-p proc))))

(defun edoc-devserver-toggle ()
  "Attiva o disattiva il devserver con `make devserver` / `make stopserver`."
  (interactive)
  (let ((default-directory (edoc--repo-path "public")))
    (if (edoc-devserver-running-p)
        ;; Spegnimento: make stopserver
        (progn
          (with-current-buffer edoc--devserver-buffer-name
            (goto-char (point-max))
            (insert "make stopserver")
            (comint-send-input)
            (message "🛑 Devserver in fase di spegnimento... (make stopserver inviato)")
            (sit-for 5.0)
	    (insert "exit")
	    (comint-send-input)
	    (kill-buffer edoc--devserver-buffer-name))
          (edoc-dashboard-refresh))
      ;; Avvio: make devserver
      (with-current-buffer (get-buffer-create edoc--devserver-buffer-name)
        (shell (current-buffer))
        (goto-char (point-max))
        (insert "make devserver")
        (comint-send-input)
        (message "🔥 Devserver avviato. Puoi vedere il sito su http://test.winstonsmith.org"))
      (edoc-dashboard-refresh))))

(provide 'edoc-dashboard)
