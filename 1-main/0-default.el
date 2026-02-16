(defvar ctl-z-map nil
  "Global Prefix Map for additional user commands.")
(define-prefix-command 'ctl-z-map)
(keymap-global-set "C-z" ctl-z-map)

(defvar thetasigma-saves-dir (concat user-emacs-directory "saves"))

;; Default customization
(leaf cus-start
  :doc "Customization of builtins and defaults"
  :tag "builtin" "internal"
  :init
  (unless (file-exists-p thetasigma-saves-dir)
    (make-directory thetasigma-saves-dir))
  :custom
  '(;; Text editing behavior
	;; (cua-mode . t) ;; if you want normal text editing
	(delete-selection-mode . t)
	(global-visual-line-mode . t)
	;; Tabs
	(indent-tabs-mode . t)
	(tab-always-indent . 'complete)
	(tab-first-completion . 'word)
	(tab-width . 4)
	;; y/n for answering yes/no questions
	(use-short-answers . t)
	;; Uniquify buffer names
	(uniquify-buffer-name-style . 'reverse)
	(uniquify-separator . " • ")
	(uniquify-after-kill-buffer-p . t)
	(uniquify-ignore-buffers-re . "^\\*")
	;; Eldoc
	(eldoc-echo-area-use-multiline-p . 8)
	(eldoc-echo-area-display-truncation-message . nil)
	;; Frame & Window Stuff
	(frame-resize-pixelwise . t)
	(default-frame-alist
	 . '((vertical-scroll-bars . nil)
		 (internal-border-width . 24)
		 (left-fringe    . 1)
		 (right-fringe   . 1)
		 (tool-bar-lines . 0)
		 (menu-bar-lines . 0)
		 (right-divider-width . 0)))
	(frame-title-format . "%b")
	(fill-column . 80)
	(split-height-threshold . 60)
	(split-width-threshold . 80)
	(window-divider-mode . nil)
	(window-min-height . 1)
	;; Windmove
	(windmove-mode . t)
	;; Backups, Savehist, Recentf
	(backup-directory-alist . `(("." . ,thetasigma-saves-dir)))
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
	(dired-dwim-target . t)
	;; GPG
	(epg-pinentry-mode . 'loopback)
	;; TRAMP
	(remote-file-name-inhibit-locks . t)
	(tramp-use-scp-direct-remote-copying . t)
	(remote-file-name-inhibit-auto-save-visited . t)
	(tramp-copy-size-limit . 1048576) ;; 1MB
	(tramp-verbose . 2))
  :bind
  '(([remap kill-buffer] . thetasigma--kill-current-buffer-and-window)
	([remap keyboard-quit] . thetasigma--keyboard-quit)
	([remap save-buffers-kill-terminal]
	 . thetasigma--delete-frame-or-kill-emacs)
	([remap quit-window] . thetasigma--kill-current-buffer-and-window)
	([remap list-buffers] . ibuffer)
	("C-x w w" . thetasigma--split-window-dwim)
	("C-x w q" . thetasigma--kill-current-buffer-and-window)
	("C-x w h" . windmove-left)
	("C-x w j" . windmove-down)
	("C-x w k" . windmove-up)
	("C-x w l" . windmove-right)
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

  ;; More TRAMP stuff
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)

  (with-eval-after-load 'tramp
	(with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options))))
