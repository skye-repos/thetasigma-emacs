;; Backups
(use-package files
  :ensure nil
  :init
  (unless (file-exists-p "~/.emacs.d/saves")
    (make-directory "~/.emacs.d/saves"))
  :custom
  (backup-directory-alist '(("." . "~/.emacs.d/saves")))
  (backup-by-copying t)
  (version-control t)
  (delete-old-versions t)
  (kept-old-versions 6)
  (kept-new-versions 9))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :custom
  (savehist-additional-variables '(kill-ring
				   command-history
				   query-replace-history
				   minibuffer-history
				   file-name-history
				   corfu-history))
  :config
  (savehist-mode t))
(setq history-length 150)
(setq kill-ring-max 50)

(put 'command-history            'history-length 10)
(put 'query-replace-history      'history-length 10)
(put 'file-name-history          'history-length 30)
(put 'minibuffer-history         'history-length 50)

;; Recentf for use with consult-buffer
(use-package recentf
  :ensure nil
  :custom
  (recentf-max-menu-items 25)
  :config
  (recentf-mode t))

(provide 'thetasigma-session)
;;; thetasigma-session.el ends here.
