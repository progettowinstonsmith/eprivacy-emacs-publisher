;;; edoc.el --- Entry point PWS -*- lexical-binding: t -*-

(require 'seq)
(require 'subr-x)

(defgroup edoc nil
  "Sistema di pubblicazione EPrivacy."
  :group 'applications)

(defcustom edoc-current-edition-path "~/production-site/private/content/2025/summer/"
  "Percorso dell’edizione EPrivacy in lavorazione."
  :type 'directory
  :group 'edoc)

(defcustom edoc-repo-private
  "git@github.com-pws:progettowinstonsmith/eprivacy-org-db.git"
  "Repository Git privato contenente i sorgenti Org."
  :type 'string
  :group 'edoc)

(defcustom edoc-repo-public
  "git@github.com-pws:progettowinstonsmith/e-privacy-site.git"
  "Repository Git pubblico contenente il sito generato."
  :type 'string
  :group 'edoc)

(defcustom edoc-production-dir
  "~/production-site"
  "Directory di lavoro contenente i repository clonati."
  :type 'directory
  :group 'edoc)

(defun edoc--repo-path (name)
  "Restituisce il path assoluto di un sotto-repo dentro `edoc-production-dir`,
oppure `~/epub-system` se NAME è \"sw\"."
  (if (string= name "sw")
      (expand-file-name "~/epub-system")
    (expand-file-name name edoc-production-dir)))

(defun edoc-clone-repos ()
  "Clona i repository se non esistono già nella `production-dir`."
  (interactive)
  (let ((default-directory edoc-production-dir))
    (unless (file-directory-p edoc-production-dir)
      (make-directory edoc-production-dir t))
    (dolist (repo `(("private" . ,edoc-repo-private)
                    ("public"  . ,edoc-repo-public)))
      (let* ((name (car repo))
             (url (cdr repo))
             (target (edoc--repo-path name)))
        (if (file-directory-p target)
            (message "Repo '%s' già clonato." name)
          (message "Clonazione di '%s' da %s..." name url)
          (call-process "git" nil "*edoc-git*" t "clone" url name)
          (message "✔ Clonato %s in %s" name target))))))

(defun edoc-pull-repos ()
  "Esegue `git pull` nei repository clonati nella production-dir."
  (interactive)
  (dolist (repo-name '("private" "public"))
    (let ((repo-path (edoc--repo-path repo-name)))
      (if (file-directory-p repo-path)
          (let ((default-directory repo-path))
            (message "Aggiornamento repo %s..." repo-name)
            (call-process "git" nil "*edoc-git*" t "pull" "--ff-only")
            (message "✔ Pull completato in %s" repo-path))
        (message "⚠ Repo %s non trovato, forse serve `edoc-clone-repos`?" repo-name)))))

(defun edoc--list-subdirectories (dir)
  "Restituisce le sottodirectory non nascoste presenti in DIR."
  (when (file-directory-p dir)
    (let ((entries (directory-files dir nil nil t)))
      (seq-filter
       (lambda (name)
         (let ((full (expand-file-name name dir)))
           (and (file-directory-p full)
                (not (string-prefix-p "." name)))))
       entries))))

(defun edoc--read-vars-keywords (vars-file)
  "Restituisce (KEYS . ALIST) leggendo tutte le keyword da VARS-FILE."
  (when (file-readable-p vars-file)
    (with-temp-buffer
      (insert-file-contents vars-file)
      (goto-char (point-min))
      (let (keys pairs)
        (while (re-search-forward "^#\\+\\([A-Z0-9_]+\\):\\s-*\\(.*\\)$" nil t)
          (let ((key (match-string 1))
                (val (string-trim (match-string 2))))
            (push key keys)
            (push (cons key val) pairs)))
        (cons (nreverse (delete-dups (nreverse keys)))
              (nreverse pairs))))))

(defun edoc--read-front-matter (file)
  "Parsa il blocco iniziale `Chiave: Valore` da FILE Markdown."
  (when (file-readable-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (result continue)
        (setq continue t)
        (while (and continue (not (eobp)))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-match "^\\s*$" line)
              (setq continue nil))
             ((string-match "^\\([A-Za-z0-9_ -]+\\):\\s-*\\(.*\\)$" line)
              (let ((key (upcase (replace-regexp-in-string " " "_" (match-string 1 line))))
                    (val (string-trim (match-string 2 line))))
                (push (cons key val) result)))
             (t (setq continue nil))))
          (forward-line 1))
        (nreverse result)))))

(defun edoc--find-session-markdown (year session public-dir)
  "Trova il file Markdown principale per l'anno YEAR e la SESSION dentro PUBLIC-DIR."
  (let* ((dir public-dir)
         (files (when (file-directory-p dir)
                  (directory-files dir nil "\\.md$" t)))
         (cfp (seq-find (lambda (f) (string= f "cfp.md")) files)))
    (cond
     (cfp (expand-file-name cfp dir))
     (files
      (let* ((year-pattern (regexp-quote year))
             (candidates (seq-filter (lambda (f)
                                       (string-match-p year-pattern (downcase f)))
                                     files)))
        (when candidates
          (expand-file-name (car candidates) dir)))))))

(defun edoc--write-vars-template (dest-file keys-alist front-alist)
  "Scrive DEST-FILE usando le KEYWORD disponibili.
KEYS-ALIST è una cons cell (KEYS . ALIST) letta da un `vars.org`
esistente; FRONT-ALIST contiene le coppie (KEY . VAL) trovate nel
Markdown."
  (let* ((keys-source (car-safe keys-alist))
         (base (cdr keys-alist))
         (front front-alist)
         (keys (delete-dups (copy-sequence (or keys-source (mapcar #'car front))))))
    (unless keys
      (user-error "Impossibile determinare le chiavi per vars.org"))
    (with-temp-file dest-file
      (dolist (key keys)
        (let* ((val (or (cdr (assoc key front))
                        (cdr (assoc key base))
                        "")))
          (insert (format "#+%s: %s\n" key val)))))))

(defun edoc-select-edition ()
  "Permette di scegliere interattivamente l'edizione corrente.
Prima propone l'anno tra le directory esistenti in `public/content/`,
poi la sessione (es. `summer`, `winter`). Aggiorna
`edoc-current-edition-path` al percorso corrispondente nel repository
privato."
  (interactive)
  (let* ((public-content (expand-file-name "content" (edoc--repo-path "public")))
         (years (edoc--list-subdirectories public-content)))
    (unless years
      (user-error "Nessun anno trovato in %s" public-content))
    (let* ((year (completing-read "Anno: " years nil t nil nil (car years)))
           (year-dir (expand-file-name year public-content))
           (sessions (edoc--list-subdirectories year-dir)))
      (unless sessions
        (user-error "Nessuna sessione trovata per l'anno %s" year))
      (let* ((session (completing-read "Sessione: " sessions nil t nil nil (car sessions)))
             (private-target (expand-file-name (format "content/%s/%s/" year session)
                                               (edoc--repo-path "private")))
             (public-target (expand-file-name (format "content/%s/%s/" year session)
                                              (edoc--repo-path "public")))
             (previous-path edoc-current-edition-path)
             (previous-vars (edoc--read-vars-keywords
                             (expand-file-name "vars.org" previous-path)))
             (front-matter (edoc--read-front-matter
                            (or (edoc--find-session-markdown year session public-target)
                                (expand-file-name "cfp.md" public-target)))))
        (unless (file-directory-p public-target)
          (message "⚠ Directory pubblica mancante: %s" public-target))
        (unless (file-directory-p private-target)
          (make-directory private-target t)
          (message "Creata directory privata: %s" private-target)
          (let ((dest-vars (expand-file-name "vars.org" private-target)))
            (if (or (car-safe previous-vars) front-matter)
                (progn
                  (edoc--write-vars-template dest-vars (or previous-vars (cons nil nil)) front-matter)
                  (message "Creato %s" dest-vars))
              (with-temp-file dest-vars
                (insert "#+TITLE: TODO\n"))
              (message "Creato %s (vuoto)" dest-vars))))
        (setq edoc-current-edition-path private-target)
        (message "edoc-current-edition-path → %s" edoc-current-edition-path)))))

(defun edoc--next-session (year session)
  "Restituisce (cons NEXT-YEAR NEXT-SESSION) per la prossima sessione.
Ciclo: summer → winter → (next year) summer."
  (pcase session
    ("summer" (cons year "winter"))
    ("winter" (cons (number-to-string (1+ (string-to-number year))) "summer"))
    (_ (cons year "summer"))))

(defun edoc-create-next-edition ()
  "Crea la prossima edizione in public/content/ANNO/SESSION/.
Basato su edoc-current-edition-path: calcola ANNO e SESSION, 
poi genera la cartella successiva nel ciclo (summer → winter → next year summer)."
  (interactive)
  (let* ((path (expand-file-name edoc-current-edition-path))
         (parts (split-string path "/" t))
         (len (length parts))
         (current-year (nth (- len 2) parts))
         (current-session (nth (- len 1) parts)))
    (unless (and current-year current-session (string-match-p "^[0-9]\\{4\\}$" current-year))
      (user-error "Non riesco a estrarre anno/sessione da: %s" path))
    (let* ((next (edoc--next-session current-year current-session))
           (next-year (car next))
           (next-session (cdr next))
           (public-base (expand-file-name "content" (edoc--repo-path "public")))
           (public-target (expand-file-name (format "%s/%s" next-year next-session) public-base)))
      (message "Creazione prossima edizione: %s/%s" next-year next-session)
      (if (file-directory-p public-target)
          (message "⚠️  Directory già esiste: %s" public-target)
        (make-directory public-target t)
        (message "✅ Creata: %s" public-target))
      (message "Usa M-x edoc-select-edition per configurare la versione privata."))))

(provide 'edoc)
