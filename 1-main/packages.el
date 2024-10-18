;; Exec Path From Shell
(unless (eq system-type 'darwin)
  (leaf exec-path-from-shell
	:ensure t
	:config
	(exec-path-from-shell-initialize)))

;; Minibuffer and goodies
(leaf vertico
  :ensure t
  :custom
  '((vertico-count . 5)
	(vertico-resize . t)
	(vertico-cycle . t)
	(vertico-mode . t))
  :config
  (with-eval-after-load 'jinx
    (add-to-list 'vertico-multiform-categories
				 '(jinx grid (vertico-grid-annotate . 20)))
	(vertico-multiform-mode 1)))

(leaf marginalia
  :ensure t
  :custom
  '((marginalia-max-relative-age . 0)
	(marginalia-field-width . 50)
	(marginalia-align . 'left)
	(marginalia-mode . t)))

(leaf vertico-posframe
  :ensure t
  :custom '((vertico-posframe-mode . t)
			(vertico-posframe-border-width . 1)
			(vertico-posframe-min-width . 75)
			(vertico-posframe-min-height . 5)))

;; Search and search matching
(leaf consult
  :ensure t
  :bind
  '(("C-s" . consult-line)
	("C-x b" . consult-buffer)))

(leaf orderless
  :ensure t
  :custom
  '((completion-styles . '(basic partial-completion emacs22 orderless))
	(completion-category-defaults . nil)
	(orderless-matching-styles . '(orderless-literal
								   orderless-prefixes
								   orderless-initialism
								   orderless-regexp))))

;; Inline completions
(leaf corfu
  :ensure t
  :custom
  '((corfu-auto . t)
	(corfu-auto-delay . 0)
	(corfu-auto-prefix . 4)
	(corfu-quit-no-match . t)
	(global-corfu-mode . t)
	(corfu-history-mode . t)
	(corfu-popupinfo-mode . t)))

;; Which Key
(leaf which-key
  :ensure t
  :custom '(which-key-mode . t))

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
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf nerd-icons-completion
  :ensure t
  :custom
  '(nerd-icons-completion-mode . t))

;; Rainbow-mode
(leaf rainbow-mode
  :ensure t)

;; Simple undo tree
(leaf vundo
  :ensure t
  :bind
  '(("s-z" . vundo)
	("C-x u" . vundo))
  :custom
  '((vundo-glyph-alist . vundo-unicode-symbols)))

;; Spell check
(leaf jinx
  :ensure t
  :hook '((emacs-startup-hook . global-jinx-mode))
  :bind '(("C-$" . jinx-correct)
          ("s-$" . jinx-correct-all)
		  ("C-s-$" . jinx-languages)))

;; Shell stuff
(leaf eat
  :ensure t
  :hook
  '((eshell-load-hook . eat-eshell-mode)
	(eshell-load-hook . eat-eshell-visual-command-mode)))

(leaf eshell-prompt-extras
  :ensure t
  :commands epe-theme-lambda
  :custom
  '((eshell-highlight-prompt . nil)
	(eshell-prompt-function . 'epe-theme-lambda)))

(leaf eshell-syntax-highlighting
  :ensure t
  :after eshell
  :custom
  '((eshell-syntax-highlighting-global-mode . t)))


;; Spacious Padding
(leaf spacious-padding
  :ensure t
  :config
  (setq spacious-padding-widths '( :right-divider-width 10
								   :scroll-bar-width 4))
  (spacious-padding-mode))
