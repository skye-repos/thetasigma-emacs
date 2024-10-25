;;; Define Key Thetasigma Variables
(defvar thetasigma-preload-dir (concat user-emacs-directory "0-preload"))

(defvar thetasigma-custom-file (concat user-emacs-directory "custom.el"))
(defcustom thetasigma-font "0xProto-16" "Font to be used in Θ Σ emacs")

;;; Minimal-emacs.d
;; Some of these have been adapted/stolen from minimal-emacs.d. Check out
;; https://github.com/jamescherti/minimal-emacs.d for more information and neat
;; tricks!

;; Garbage collection
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; Language Settings
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Performance
(setq load-prefer-newer t)
(setq inhibit-compacting-font-caches t)

;; Startup
(setq-default inhibit-startup-message t
			  inhibit-startup-screen t
			  inhibit-startup-buffer-menu t
			  inhibit-startup-echo-area-message t)

(setq menu-bar-mode nil)
(setq tool-bar-mode nil)
(setq scroll-bar-mode nil)

;;; Thetasigma stuff
;; The GNU reference manual
;; (https://www.gnu.org/software/emacs/manual/html_node/emacs/Early-Init-File.html)
;; urges users to place code that needs to be loaded before the GUI is
;; initialized here.  I have strayed minimally from this only in an attempt to
;; fix issues regarding the emacs daemon. Since the daemon can be initialized
;; without initializing a frame, I notice inconsistencies with fonts and values
;; in custom.el

;; Load all elisp scripts in a folder
(defun thetasigma--load-files (dir)
  "Ensure DIR exists, and load all elisp files in DIR."
  (if (file-exists-p dir)
	  (let* ((files (directory-files dir t ".el$"))
			 (value nil))
		(dolist (file files value)
		  (load-file file)))
	(error "Directory %s does not exist" dir)))

;; Theme & Font
;; Load the Θ Σ Emacs Theme.
(add-to-list 'custom-theme-load-path (concat user-emacs-directory "themes"))
(load-theme 'thetasigma t)

;; 0xProto is a monospaced font that makes reading prose easier for me. Change
;; it by changing the =thetasigma-font-string= variable. Reference the GNU
;; manual on how to set fonts in Emacs:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html

(if (daemonp)
	(add-to-list 'after-make-frame-functions
				 (lambda (frame)
				   (with-selected-frame frame)
				   (when (display-graphic-p frame)
					 (set-face-font 'default thetasigma-font frame))))
  (set-face-font 'default thetasigma-font))

;; Set folder to dump M-x customize vars. Loading this as late as possible in
;; early init to give a chance for the user's customizations to overwrite mine.
(setq custom-file thetasigma-custom-file)
(unless (file-exists-p custom-file)
  (make-empty-file custom-file))
(load custom-file)

;;; package.el is also loaded only after the early-init.el file. Thus, one can
;;; customize package.el variables without loading it. Further customizations
;;; that need to be made that DO NOT make calls to package.el, can also be
;;; loaded here from the =preload/= directory.
(setq package-enable-at-startup nil)
(setq package-quickstart nil)

(customize-set-variable
   'package-archives '(("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")
					   ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(thetasigma--load-files thetasigma-preload-dir)
