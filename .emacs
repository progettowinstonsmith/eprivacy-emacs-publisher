;; -*- lexical-binding: t -*-

;; Carica il pacchetto di pubblicazione locale


(require 'org-element)

(add-to-list 'load-path "~/epub-system/")

(require 'pws-common)
(require 'pws-org)
(require 'pws-emacs-org)
(require 'edoc)
(require 'edoc-dashboard)

(setq edoc-email-escluse '("marcoc@marcoc.it" "emmanuele@exedre.org"))

;; Non far mai apparire la schermata di benvenuto o il *scratch*
(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)
(setq initial-buffer-choice t)



;; Apriamo la dashboard dopo l’avvio
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Chiude tutto tranne la dashboard
            (dolist (buf '("*GNU Emacs*" "*scratch*"))
              (when (get-buffer buf)
                (kill-buffer buf)))
            ;; Mostra dashboard
            (edoc-dashboard-refresh)
            ;; Passa alla dashboard (e chiude tutte le finestre tranne una)
            (delete-other-windows)
            (switch-to-buffer "*PWS Dashboard*")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(edoc-current-edition-path "~/production-site/private/content/2025/winter/")
 '(package-selected-packages '(magit org)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
