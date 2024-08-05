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

(leaf expreg
  :ensure t
  :bind
  '(("C-=" . expreg-expand)
	("C--" . expreg-contract)))

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
