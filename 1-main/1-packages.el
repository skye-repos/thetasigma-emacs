;; Exec Path From Shell
(unless (eq system-type 'darwin)
  (leaf exec-path-from-shell
	:ensure t
	:config
	(exec-path-from-shell-initialize)))

;; Spell check
(leaf jinx
  :ensure t
  :hook '((emacs-startup-hook . global-jinx-mode))
  :bind '(("C-$" . jinx-correct)
          ("s-$" . jinx-correct-all)
		  ("C-s-$" . jinx-languages)))

;; Rainbow-mode
(leaf rainbow-mode
  :ensure t)

;; Persistent undo history
(leaf undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode t))

;; Spacious Padding
(leaf spacious-padding
  :ensure t
  :custom
  '((spacious-padding-widths . '( :right-divider-width 10
								  :scroll-bar-width 4)))
  :config
  (spacious-padding-mode t))
