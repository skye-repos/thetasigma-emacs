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

;; Tempel
(leaf tempel
  :ensure t
  :bind
  '(("C-<tab>" . tempel-next)
	("M-<tab>" . tempel-previous))
  :custom
  '((tempel-trigger-prefix . "<"))
  :config
  (defun tempel-setup-capf ()
    "Setup templ Capf endpoint."
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  '((conf-mode-hook . tempel-setup-capf)
	(prog-mode-hook . tempel-setup-capf)
	(text-mode-hook . tempel-setup-capf)))

(leaf tempel-collection
  :ensure t)

;; Which Key
(leaf which-key
  :ensure t
  :custom '(which-key-mode . t))
