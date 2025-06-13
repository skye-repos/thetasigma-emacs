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
