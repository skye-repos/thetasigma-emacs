(use-package elec-pair
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

  (org-mode . (lambda ()
                (setq-local electric-pair-pairs (append electric-pair-pairs '((?$ . ?$))))))
  :config (electric-pair-mode t))

(use-package electric
  :ensure nil
  :config (electric-indent-mode t))

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
  :hook (prog-mode . flymake-mode))

(use-package package-lint-flymake
  :after flymake
  :hook (elisp-mode . package-lint-flymake-setup))

(use-package expreg
  :bind
  ("C-=" . expreg-expand)
  ("C--" . expreg-contract))

(use-package magit)

(provide 'thetasigma-prog)
;;; thetasigma-prog.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars)
;; End:

