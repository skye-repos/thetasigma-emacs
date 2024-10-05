;; LSP-mode instead of eglot (needs solving)
(leaf lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  '((lsp-mode-hook . lsp-enable-which-key-integration)))

(leaf lsp-ui
  :ensure t)

;; Paired parens
(leaf elec-pair
  :custom
  '((electric-pair-mode . t))
  :hook
  '((org-mode-hook . (lambda ()
					   (setq-local
						electric-pair-inhibit-predicate
						`(lambda (c)
						   (if (char-equal c ?<)
							   t
							 (,electric-pair-inhibit-predicate c))))))

	(org-mode-hook . (lambda ()
					   (setq-local
						electric-pair-pairs
						(append electric-pair-pairs '((?$ . ?$))))))))

;; Smart Indent
(leaf electric
  :custom
  '((electric-indent-mode . t)))

;; Semantic Expansion of region
(leaf expreg
  :ensure t
  :bind
  '(("C-=" . expreg-expand)
	("C--" . expreg-contract)))

;; Git
(leaf magit
  :ensure t
  :custom
  (magit-process-password-prompt-regexps .
										 '("^\\(Enter \\)?[Pp]assphrase\\( for \\(RSA \\)?key '.*'\\)?: ?$"
										   ;; match-group 99 is used to identify a host
										   "^\\(Enter \\)?[Pp]assword\\( for '\\(?99:.*\\)'\\)?: ?$"
										   "^.*'s password: ?$"
										   "^Yubikey for .*: ?$"
										   "^Enter PIN for '.*': ?$")))
