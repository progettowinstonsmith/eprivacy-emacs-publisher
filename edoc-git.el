;;; edoc-git.el --- Stato Git per PWS -*- lexical-binding: t -*-

(defun edoc-git-last-commit-summary (repo-path)
  "Restituisce un riassunto dell’ultimo commit nel repository in REPO-PATH."
  (when (file-directory-p repo-path)
    (let ((default-directory repo-path))
      (with-temp-buffer
        (call-process "git" nil t nil "log" "-1" "--pretty=format:%an (%ar)")
        (string-trim (buffer-string))))))

(defun edoc-git-up-to-date-p (repo-path)
  "Restituisce t se il repository locale è aggiornato rispetto al remoto."
  (when (file-directory-p repo-path)
    (let ((default-directory repo-path))
      (call-process "git" nil nil nil "fetch")
      (with-temp-buffer
        (call-process "git" nil t nil "status" "-uno")
        (goto-char (point-min))
        (not (re-search-forward "Your branch is behind" nil t))))))

(defun edoc-git-repo-dirty-p (repo-path)
  "Restituisce t se il repository contiene modifiche locali non committate."
  (when (file-directory-p repo-path)
    (let ((default-directory repo-path))
      ;; git diff --quiet restituisce 1 se ci sono modifiche
      (= (call-process "git" nil nil nil "diff" "--quiet") 1))))


(provide 'edoc-git)
