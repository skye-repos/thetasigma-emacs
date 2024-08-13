(leaf lsp-mode
  :ensure t
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-l")
  :hook
  '((lsp-mode-hook . lsp-enable-which-key-integration)))

(leaf lsp-ui
  :ensure t)
