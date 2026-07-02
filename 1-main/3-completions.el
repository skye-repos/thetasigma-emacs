;; (leaf completion-preview-mode
;;   :ensure nil
;;   :hook
;;   '((prog-mode-hook . completion-preview-mode)
;; 	(text-mode-hook . completion-preview-mode)
;; 	(eshell-mode-hook . completion-preview-mode))
;;   :bind
;;   '(:completion-preview-active-mode-map
;; 	("<tab>" . completion-preview-insert))
;;   :custom
;;   '((completion-preview-minimum-symbol-length . 2)))

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

;; Yasnippet
(leaf yasnippet
  :ensure t
  :hook
  '((prog-mode-hook . yas-minor-mode)
	(LaTeX-mode-hook . yas-minor-mode)
	(latex-mode-hook . yas-minor-mode)))

(leaf yasnippet-snippets
  :ensure t)

;; Which Key
(leaf which-key
  :ensure t
  :custom '(which-key-mode . t))
