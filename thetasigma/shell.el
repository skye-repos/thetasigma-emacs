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
