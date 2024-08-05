(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; Theme
(add-to-list 'custom-theme-load-path user-emacs-directory)
(load-theme 'thetasigma t)

(defun thetasigma--load-files (dir)
  "Ensure DIR exists, and load all elisp files in DIR."
  (if (file-exists-p dir)
	  (let* ((files (directory-files dir t ".el$"))
			 (value nil))
		(dolist (file files value)
		  (load-file file)))
	(error "Directory %s does not exist" dir)))

(thetasigma--load-files "~/.emacs.d/thetasigma-preload")
(thetasigma--load-files "~/.emacs.d/thetasigma")
(thetasigma--load-files "~/.emacs.d/thetasigma-user")
