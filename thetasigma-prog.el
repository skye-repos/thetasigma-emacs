(leaf elec-pair
  :custom
  '((electric-pair-mode . t))
  :hook
  '((org-mode-hook . (lambda ()
					   (setq-local electric-pair-inhibit-predicate ?<)))

	(org-mode-hook . (lambda ()
					   (setq-local electric-pair-pairs
								   (append electric-pair-pairs '((?$ . ?$))))))))

(leaf electric
  :custom
  '((electric-indent-mode . t)))

(leaf flymake
  :commands flymake-mode
  :custom
  '((flymake-fringe-indicator-position . 'left-fringe)
	(flymake-suppress-zero-counters . t)
	(flymake-start-on-flymake-mode . t)
	(flymake-no-changes-timeout . nil)
	(flymake-start-on-save-buffer . t)
	(flymake-proc-compilation-prevents-syntax-check . t)
	(flymake-wrap-around . nil))
  :hook
  '((prog-mode-hook . flymake-mode)))

(leaf package-lint-flymake
  :ensure t
  :after flymake
  :hook
  '((elisp-mode-hook . package-lint-flymake-setup)))

(leaf expreg
  :ensure t
  :bind
  '(("C-=" . expreg-expand)
	("C--" . expreg-contract)))

(leaf magit
  :ensure t)
