;;; Code
;; Minibuffer and goodies
(leaf vertico
  :custom
  '((vertico-count . 8)
	(vertico-resize . t)
	(vertico-cycle . nil)
	(vertico-mode . t))
  :config
  (with-eval-after-load 'jinx
    (add-to-list 'vertico-multiform-categories
				 '(jinx grid (vertico-grid-annotate . 20)))
	(vertico-multiform-mode 1)))

(leaf marginalia
  :custom
  '((marginalia-max-relative-age . 0)
	(marginalia-align . 'right)
	(marginalia-mode . t)))

(leaf vertico-posframe
  :custom '(vertico-posframe-mode . t))

;; Search and search matching
(leaf consult
  :bind
  '(("C-s" . consult-line)
	("C-x b" . consult-buffer)))

(leaf orderless
  :custom
  (completion-styles . '(orderless))
  (completion-category-defaults . nil)
  (orderless-matching-styles '(orderless-literal
							   orderless-prefixes
							   orderless-initialism
							   orderless-regexp)))

;; Inline completions
(leaf corfu
  :custom
  '((corfu-auto . t)
	(corfu-auto-delay . 0)
	(corfu-auto-prefix . 2)
	(corfu-quit-no-match . t)
	(global-corfu-mode . t)
	(corfu-history-mode . t)
	(corfu-popupinfo-mode . t)))

;; Which Key
(leaf which-key
  :custom '(which-key-mode . t))

;; Icons
(leaf nerd-icons
  :init
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd))
         (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
           (nerd-icons-install-fonts)))
        ((eq system-type 'darwin)
         (unless (file-exists-p "~/Library/Fonts/NFM.ttf")
           (nerd-icons-install-fonts)))))

(leaf nerd-icons-dired
  :hook
  '(dired-mode-hook . nerd-icons-dired-mode))

(leaf nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(leaf nerd-icons-completion
  :custom
  '(nerd-icons-completion-mode . t))

;; Rainbow-mode
(leaf rainbow-mode)
