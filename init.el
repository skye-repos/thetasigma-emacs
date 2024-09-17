;; Setup leaf for easy package configurations
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
					   ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; Load all everything else.
(defun thetasigma--load-files (dir)
  "Ensure DIR exists, and load all elisp files in DIR."
  (if (file-exists-p dir)
	  (let* ((files (directory-files dir t ".el$"))
			 (value nil))
		(dolist (file files value)
		  (load-file file)))
	(error "Directory %s does not exist" dir)))

(thetasigma--load-files "~/.config/emacs/preload")
(thetasigma--load-files "~/.config/emacs/thetasigma")
(thetasigma--load-files "~/.config/emacs/user")
