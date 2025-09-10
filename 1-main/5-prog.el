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
  :config
  (setq transient-default-level 5))

(leaf magit-delta
  :ensure t
  :hook
  '((magit-mode-hook .
					 (lambda () (magit-delta-mode t)))))
