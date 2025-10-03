;;; edoc.el --- Entry point PWS -*- lexical-binding: t -*-

(require 'seq)

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
                                              (edoc--repo-path "public"))))
        (unless (file-directory-p public-target)
          (message "⚠ Directory pubblica mancante: %s" public-target))
        (unless (file-directory-p private-target)
          (make-directory private-target t)
          (message "Creata directory privata: %s" private-target))
        (setq edoc-current-edition-path private-target)
        (message "edoc-current-edition-path → %s" edoc-current-edition-path)))))

(provide 'edoc)
