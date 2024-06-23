;; A better way to use C-g that is a little more context sensitive
(defun thetasigma--keyboard-quit ()
  "Quit current context.

   This function is a combination of `keyboard-quit' and
   `keyboard-escape-quit' with some parts omitted and some custom
   behavior added.

   Courtesy of u/clemera from reddit"
  (interactive)
  (progn (cond
		  ;; Avoid adding the region to the window selection.
		  ((region-active-p)
		   (setq saved-region-selection nil)
		   (let (select-active-regions) (deactivate-mark)))
		  ;; If the last command was =mode-exited= then return nil.
		  ((eq last-command 'mode-exited) nil)
		  ;; If you have accidenally added a bunch of C-u's, get rid of them. Here, current-prefix-arg returns a non-nil value (=> conditional)
		  (current-prefix-arg nil)
		  ;; Prevent quit-keyboard being used in a macro. Can be annoying. Here, defining-kbd-macro returns a non-nil value (=> conditional)
		  (defining-kbd-macro (message (substitute-command-keys
										"Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
							  (cancel-kbd-macro-events))
		  ;; Default case
		  (t (keyboard-quit)))

		 ;; Kill all child-frames
		 (cond
		  ((frame-parent)
		   (delete-frame))
		  ((not (frame-parent))
		   (dolist (frame (frame-list))
			 (if (frame-parameter frame 'parent-frame)
				 (delete-frame frame t))))
		  ;; Default case
		  (t (keyboard-quit)))))

;; Basic Utility Keybinds
(use-package simple
  :ensure nil
  :bind
  ( :map global-map
	([remap kill-buffer] . kill-current-buffer)
	([remap keyboard-quit] . thetasigma--keyboard-quit)
	:map ctl-z-map
	("C-<SPC>" . fixup-whitespace)))

(use-package files
  :ensure nil
  :bind
  ("<f5>" . revert-buffer))

;; Rebind help functions sensibly
(use-package help-fns
  :ensure nil
  :bind
  ( :map help-map
    ("F" . describe-face)
    ("s" . describe-symbol)
    ("S" . describe-syntax)
    ("p" . describe-package)
    ("P" . describe-personal-keybinds)
    ("g" . nil)))

(use-package info
  :ensure nil
  :bind
  ( :map help-map
    ("C-f" . Info-goto-emacs-command-node)
    ("C-k" . Info-goto-emacs-key-command-node)
    ("C-m" . info-display-manual)
    ("C-r" . info-emacs-manual)
    ("C-b" . info-emacs-bug)))

(use-package info-look
  :ensure nil
  :bind
  ( :map help-map
    ("C-S-f" . info-lookup-file)
    ("C-S-s" . info-lookup-symbol)))

;; Text Editing Changes
;(cua-mode t)
(delete-selection-mode t)
(global-visual-line-mode t)
(setq x-underline-at-descent-line t)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Tabs
(customize-set-value 'tab-always-indent 'complete)
(customize-set-value 'tab-width 4)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-command-modifier 'meta
        mac-option-modifier 'super)
  
  (cond ((file-exists-p "/opt/local/bin/gls")
         (setq insert-directory-program "/opt/local/bin/gls"))
        ((file-exists-p "/opt/homebrew/bin/gls")
         (setq insert-directory-program "/opt/homebrew/bin/gls"))))

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
	  (lambda () (setq buffer-display-table (make-display-table))))

(provide 'thetasigma-defaults)
;;; thetasigma-defaults.el ends here.
