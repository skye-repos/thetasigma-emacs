;; Load the Θ Σ Emacs Theme.
(add-to-list 'custom-theme-load-path "~/.config/emacs/thetasigma/")
(load-theme 'thetasigma t)

(leaf spacious-padding
  :ensure t
  :custom
  '((spacious-padding-subtle-mode-line . t)
	(spacious-padding-widths . '( :right-divider-width 10
								  :scroll-bar-width 4)))
  :config
  (spacious-padding-mode))

;; Not necessary but recommended. Pick the font of your choice.
(set-face-attribute 'default nil :family "0xProto" :height 150)
