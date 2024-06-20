;;; Code:
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'treesit-language-source-alist '(elisp "https://github.com/Wilfred/tree-sitter-elisp"))
  (add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
  (add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
  (unless (treesit-language-available-p 'elisp)
    (treesit-install-language-grammar 'elisp))
  (unless (treesit-language-available-p 'c)
    (treesit-install-language-grammar 'c))
  (unless (treesit-language-available-p 'bash)
    (treesit-install-language-grammar 'bash))
  )

(use-package elisp-mode
  :ensure nil
  :bind
  ( :map ctl-z-map
	("C-e" . eval-defun))
  )

(use-package newcomment
  :ensure nil
  :bind
  ( :map ctl-z-map
	(";" . comment-dwim))
  )

(use-package elec-pair
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

  (org-mode . (lambda ()
                (setq-local electric-pair-pairs (append electric-pair-pairs '((?$ . ?$))))))
  :config
  (electric-pair-mode 1)
  )

(use-package flymake
  :commands flymake-mode
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  :hook
  (prog-mode . flymake-mode))

(use-package package-lint
  :after flymake
  :config
  :hook
  (elisp-mode . package-lint-flymake-setup))

(use-package magit
  :defines magit-view-git-manual-method)

(provide 'thetasigma-prog)
;;; thetasigma-prog.el ends here
