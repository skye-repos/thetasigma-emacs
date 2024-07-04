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

(load-file "~/.emacs.d/thetasigma-mode-line.el")
(load-file "~/.emacs.d/thetasigma-system.el")
(load-file "~/.emacs.d/thetasigma-functions.el")

(define-prefix-command 'ctl-z-map)
(keymap-global-set "C-Z" ctl-z-map)

;; Default customization
(leaf cus-start
  :doc "Customization of builtins and defaults"
  :tag "builtin" "internal"
  :init
  (unless (file-exists-p "~/.emacs.d/saves")
    (make-directory "~/.emacs.d/saves"))
  (unless (file-exists-p "~/.emacs.d/custom.el")
	(write-file "~/.emacs.d/custom.el"))
  :custom
  '((custom-file . "~/.emacs.d/custom.el")
	(inhibit-startup-message . t)
	(inhibit-startup-screen . t)
	(inhibit-startup-buffer-menu . t)
	(inhibit.startup-echo-area-message . t)
	;; Text editing behavior
	;; (cua-mode . t) ;; if you want normal text editing
	(delete-selection-mode . t)
	(global-visual-line-mode . t)
	;; Tabs
	(tab-always-indent . 'complete)
	(tab-width . 4)
	;; y/n for answering yes/no questions
	(use-short-answers . t)
	;; Uniquify buffer names
	(uniquify-buffer-name-style . 'reverse)
	(uniquify-separator . " • ")
	(uniquify-after-kill-buffer-p . t)
	(uniquify-ignore-buffers-re . "^\\*")
	;; Frame & Window Stuff
	(default-frame-alist
	 . '((fullscreen . fullboth)
		 (vertical-scroll-bars . nil)
		 (internal-border-width . 24)
		 (left-fringe    . 1)
		 (right-fringe   . 1)
		 (tool-bar-lines . 0)
		 (menu-bar-lines . 0)
		 (right-divider-width . 0)))
	(frame-title-format . nil)
	(fill-column . 80)
	(window-divider-mode . nil)
	(window-min-height . 1)
	(set-scroll-bar-mode . nil)
	(tool-bar-mode . nil)
	(menu-bar-mode . nil)
	;; Windmove
	(windmove-mode . t)
	;; Backups, Savehist, Recentf
	(backup-directory-alist . '(("." . "~/.emacs.d/saves")))
	(backup-by-copying . t)
	(version-control . t)
	(delete-old-versions . t)
	(kept-old-versions . 6)
	(kept-new-versions . 9)
	(savehist-additional-variables
	 . '(kill-ring
		 command-history
		 query-replace-history
		 minibuffer-history
		 file-name-history
		 corfu-history))
	(history-length . 150)
	(kill-ring-max . 50)
	(savehist-mode . t)
	(recentf-max-menu-items . 25)
	(recentf-mode . t)
	;; Dired
	(dired-recursive-copies . 'always)
	(dired-recursive-deletes . 'always)
	(delete-by-moving-to-trash . t)
	(dired-listing-switches
	 . "-al --group-directories-first --time-style=iso")
	(dired-dwim-target . t))
  :bind
  '(([remap kill-buffer] . kill-current-buffer)
	([remap keyboard-quit] . thetasigma--keyboard-quit)
	([remap save-buffers-kill-terminal]
	 . thetasigma--delete-frame-or-kill-emacs)
	([remap quit-window] . thetasigma--quit-window)
	("s-<up>" . windmove-up)
	("s-<down>" . windmove-down)
	("s-<left>" . windmove-left)
	("s-<right>" . windmove-right)
	("s-k" . windmove-up)
	("s-j" . windmove-down)
	("s-h" . windmove-left)
	("s-l" . windmove-right)
	("<f5>" . revert-buffer)
	("C-h F" . describe-face)
	("C-h s" . describe-symbol)
	("C-h S" . describe-syntax)
	("C-h p" . describe-package)
	("C-h P" . describe-personal-keybinds)
	("C-h g" . nil)
	("C-h C-f" . Info-goto-emacs-command-node)
	("C-h C-k" . Info-goto-emacs-key-command-node)
	("C-h C-m" . info-display-manual)
	("C-h C-r" . info-emacs-manual)
	("C-h C-b" . info-emacs-bug)
	("C-h C-S-f" . info-lookup-file)
	("C-h C-S-s" . info-lookup-symbol)
	("C-z ;" . comment-dwim)
	("C-z C-e" . eval-defun)
	("C-z C-<SPC>" . fixup-whitespace))
  :hook
  '((dired-mode-hook . dired-hide-details-mode))
  :config
  (put 'command-history            'history-length 10)
  (put 'query-replace-history      'history-length 10)
  (put 'file-name-history          'history-length 30)
  (put 'minibuffer-history         'history-length 50)
  (load custom-file))

;; Now load the rest of the stuff
(load-file "~/.emacs.d/thetasigma-packages.el")
(load-file "~/.emacs.d/thetasigma-prog.el")
(load-file "~/.emacs.d/thetasigma-writing.el")
(load-file "~/.emacs.d/thetasigma-hyperbole.el")

;; Not necessary but recommended. Pick the font of your choice.
(set-face-attribute 'default nil :family "DM Mono" :height 150)

;; Theme refresh function
(defun skye/refresh-theme ()
  "Helper function when working with a theme. Please don't use."
  (interactive)
  (load-file "~/.emacs.d/thetasigma-theme.el")
  (disable-theme 'thetasigma)
  (enable-theme 'thetasigma)
  (vertico-posframe-cleanup))
(keymap-global-set "<f4>" 'skye/refresh-theme)
