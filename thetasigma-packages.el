;; Minibuffer and goodies
(use-package vertico
  :custom
  (vertico-count 8)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-mode t)
  )

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-mode t)
  )

(use-package vertico-posframe
  :custom (vertico-posframe-mode t))

;; Search and search matching
(use-package consult
  :bind
  (:map global-map
        ("C-s" . consult-line)
        ("C-x b" . consult-buffer))
  )

(use-package orderless
  :custom
  (completion-styles '(orderless))  
  (completion-category-defaults nil)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)
   )
  )

;; Inline completions
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  (global-corfu-mode t)
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  )

;; Dired
(use-package dired
  :ensure nil
  :custom
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'always)
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-al --group-directories-first --time-style=iso")
  (dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode))

;; Which Key
(use-package which-key
  :custom (which-key-mode t))

;; Icons
(use-package nerd-icons
  :init
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd))
         (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
           (nerd-icons-install-fonts)))
        ((eq system-type 'darwin)
         (unless (file-exists-p "~/Library/Fonts/NFM.ttf")
           (nerd-icons-install-fonts))))
  )

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode)
  )

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)
  )

;; Raibow-mode
(use-package rainbow-mode)

(provide 'thetasigma-packages)
;;; thetasigma-packages.el ends here.
