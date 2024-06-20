;;; thetasigma-defaults --- default emacs settings
;;; Commentary:
;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / Θ Σ - Emacs for Memacs
;; Copyright (C) 2024 - Θ Σ developers
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; Code:

;; A better way to use C-g that is a little more context sensitive
(defun thetasigma--keyboard-quit ()
  "Quit current context.

   This function is a combination of `keyboard-quit' and
   `keyboard-escape-quit' with some parts omitted and some custom
   behavior added.

   This function is courtesy of u/clemera from reddit"
  (interactive)
  (cond
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
   ;; Kill all child-frames
   ((frame-parent)
    (delete-frame))
   ((not (frame-parent))
    (dolist (frame (frame-list))
      (if (frame-parameter frame 'parent-frame)
          (delete-frame frame t)))
    )
   ;; Default case
   (t (keyboard-quit))
   )
  )

(use-package simple
  :ensure nil
  :bind
  ( :map global-map
	([remap kill-buffer] . kill-current-buffer)
	([remap keyboard-quit] . thetasigma--keyboard-quit)
	:map ctl-z-map
	("C-<SPC>" . fixup-whitespace)))

;; Put customized variables in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; Text Editing Changes
;(cua-mode t)
(delete-selection-mode t)
(global-visual-line-mode t)
(setq x-underline-at-descent-line t)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Tabs
(setq tab-always-indent 'complete)

;; Tab.space equivalence
(setq-default tab-width 4)

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
        mac-option-modifier nil)
  
  (cond ((file-exists-p "/opt/local/bin/gls")
         (setq insert-directory-program "/opt/local/bin/gls"))
        ((file-exists-p "/opt/homebrew/bin/gls")
         (setq insert-directory-program "/opt/homebrew/bin/gls"))))

;; Fix bug on OSX in term mode & zsh (spurious % after each command)
(add-hook 'term-mode-hook
	  (lambda () (setq buffer-display-table (make-display-table))))

(provide 'thetasigma-defaults)
;;; thetasigma-defaults.el ends here.
