;;; PWS Dashboard Launcher Init

(add-to-list 'load-path "~/epub-system/")

(require 'roman-numerals)
(require 'edoc-vars)
(require 'edoc-dashboard)
(require 'edoc)

(setq edoc-current-edition-path "~/production-site/private/content/2025/winter/")

(dolist (buf '("*GNU Emacs*" "*scratch*"))
  (when (get-buffer buf)
    (kill-buffer buf)))

(edoc-dashboard-refresh)
(delete-other-windows)
(switch-to-buffer "*PWS Dashboard*")
