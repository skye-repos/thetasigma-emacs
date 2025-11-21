;; Icons
(leaf nerd-icons
  :ensure t
  :init
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd))
         (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
           (nerd-icons-install-fonts)))
        ((eq system-type 'darwin)
         (unless (file-exists-p "~/Library/Fonts/NFM.ttf")
           (nerd-icons-install-fonts)))))

(leaf nerd-icons-dired
  :ensure t
  :hook
  '(dired-mode-hook . nerd-icons-dired-mode))

(leaf nerd-icons-corfu
  :ensure t
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf nerd-icons-completion
  :ensure t
  :after vertico
  :hook
  '((marginalia-mode-hook . nerd-icons-completion-marginalia-setup))
  :config
  (nerd-icons-completion-mode t))
