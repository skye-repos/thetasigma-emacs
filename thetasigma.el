;;; Code:
(defvar thetasigma-dir "~/.emacs.d/thetasigma-emacs/"
  "Directory that Θ Σ - Emacs was cloned into.")

;; Theme
(add-to-list 'custom-theme-load-path thetasigma-dir)
(load-theme 'thetasigma t)

;; Startup Stuff
(customize-set-value 'inhibit-startup-message t)
(customize-set-value 'inhibit-startup-screen t)
(customize-set-value 'inhibit-startup-buffer-menu t)
(customize-set-value 'inhibit-startup-echo-area-message t)

;; Custom Prefix used in key binds
(define-prefix-command 'ctl-z-map)
(keymap-global-set "C-z" ctl-z-map)

(require 'thetasigma-defaults)
(require 'thetasigma-system)
(require 'thetasigma-session)

;; Load Basic Convenience Packages
(require 'thetasigma-packages)
(require 'thetasigma-prog)

(require 'thetasigma-writing)

(provide 'thetasigma)
;;; thetasigma.el ends here.
