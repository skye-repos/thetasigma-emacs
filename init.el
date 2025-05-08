;; Setup leaf for easy package configurations
(eval-and-compile
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

;; Load Main Config and User Configs
(defvar thetasigma-main-dir (concat user-emacs-directory "1-main"))
(thetasigma--load-files thetasigma-main-dir)

(defvar thetasigma-user-dir (concat user-emacs-directory "2-user"))
(if (file-exists-p thetasigma-user-dir)
	(thetasigma--load-files thetasigma-user-dir)
  (make-directory thetasigma-user-dir))

;; Set folder to dump M-x customize vars. Loading this as late as possible in
;; early init to give a chance for the user's customizations to overwrite mine.
(defvar thetasigma-custom-file (concat user-emacs-directory "custom.el"))
(setq custom-file thetasigma-custom-file)

(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load custom-file)
