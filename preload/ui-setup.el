;; Load the Θ Σ Emacs Theme.
(add-to-list 'custom-theme-load-path "~/.config/emacs/preload/")
(load-theme 'thetasigma t)



;; Not necessary but recommended. Pick the font of your choice.
(if (daemonp)
	(add-to-list 'after-make-frame-functions
				 (lambda (frame)
				   (with-selected-frame frame)
				   (when (display-graphic-p frame)
					 (set-face-font 'default "0xProto-16" frame))))
  (set-face-font 'default "0xProto-16"))

