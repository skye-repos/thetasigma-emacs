;; Load the Θ Σ Emacs Theme.
(add-to-list 'custom-theme-load-path "~/.emacs.d/thetasigma-user/")
(load-theme 'thetasigma t)

;; Not necessary but recommended. Pick the font of your choice.
(set-face-attribute 'default nil :family "0xProto" :height 150)
