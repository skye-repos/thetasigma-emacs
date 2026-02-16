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
	(corfu-auto-prefix . 2)
	(corfu-quit-no-match . t)
	(global-corfu-mode . t)
	(corfu-history-mode . t)
	(corfu-popupinfo-mode . t)))

(leaf cape
  :ensure t
  :config
   (add-hook 'completion-at-point-functions #'cape-dabbrev)
   (add-hook 'completion-at-point-functions #'cape-file)
   (add-hook 'completion-at-point-functions #'cape-tex))

;; ;; Tempel
;; (leaf tempel
;;   :ensure t
;;   :bind
;;   '(("C-c i" . tempel-complete)
;; 	("C-<tab>" . tempel-next)
;; 	("S-<tab>" . tempel-previous))
;;   :custom
;;   '((tempel-trigger-prefix . "<"))
;;   :config
;;   (defun tempel-setup-capf ()
;;     "Setup templ Capf endpoint."
;;     (setq-local completion-at-point-functions
;; 		(cons #'tempel-expand
;;                       completion-at-point-functions)))
;;   :hook
;;   '((conf-mode-hook . tempel-setup-capf)
;; 	(prog-mode-hook . tempel-setup-capf)
;; 	(text-mode-hook . tempel-setup-capf)))

;; (leaf tempel-collection
;;   :ensure t)

;; Yasnippet
(leaf yasnippet
  :ensure t
  :hook
  '((prog-mode-hook . yas-minor-mode)
	(LaTeX-mode-hook . yas-minor-mode)
	(latex-mode-hook . yas-minor-mode)))

(leaf yasnippet-snippets
  :ensure t)

(leaf yasnippet-capf
  :ensure t)

;; Which Key
(leaf which-key
  :ensure t
  :custom '(which-key-mode . t))
