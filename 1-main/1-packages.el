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

;; ;; Simple undo tree
;; (leaf vundo
;;   :ensure t
;;   :bind
;;   '(("s-z" . vundo)
;; 	("C-x u" . vundo))
;;   :custom
;;   '((vundo-glyph-alist . vundo-unicode-symbols)))

;; Persistent undo history
(leaf undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode t))

;; Shell stuff
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


;; Spacious Padding
(leaf spacious-padding
  :ensure t
  :custom
  '((spacious-padding-widths . '( :right-divider-width 10
								  :scroll-bar-width 4)))
  :config
  (spacious-padding-mode t))
