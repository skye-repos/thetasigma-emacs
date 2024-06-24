;;; thetasigma-defaults.el --- The bare minimum
;; -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))
;; Homepage: https://github.com/skye-repos/thetasigma-emacs
;; Keywords: config, custom

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is just the absolute minimum stuff I need.  I also remap the
;; help functions and such.

;;; Code:
(require 'dash)

;; A better way to use C-g that is a little more context sensitive
(defun thetasigma--keyboard-quit ()
  "Quit current context.

   This function is a combination of `keyboard-quit' and
   `keyboard-escape-quit' with some custom behavior.

   Courtesy of u/clemera from Reddit"
  (interactive)
  (progn (cond
	  ;; Avoid adding the region to the window selection.
	  ((region-active-p)
	   (setq saved-region-selection nil)
	   (let (select-active-regions) (deactivate-mark)))
	  ;; If the last command was =mode-exited= then return nil.
	  ((eq last-command 'mode-exited) nil)
	  ;; If you have accidentally added a bunch of C-u's, get rid
	  ;; of them. Here, current-prefix-arg returns a non-nil value
	  ;; (=> conditional)
	  (current-prefix-arg nil)
	  ;; Prevent quit-keyboard being used in a macro. Can be
	  ;; annoying. Here, defining-kbd-macro returns a non-nil value
	  ;; (=> conditional)
	  (defining-kbd-macro
	   (message
	    (substitute-command-keys
	     "Quit is ignored during macro definition, use \\[kmacro-end-macro]
		     if you want to stop macro definition"))
	   (cancel-kbd-macro-events))
	  ;; Default case
	  (t (keyboard-quit)))

	 ;; Kill all child-frames
	 (--map-when (eq (frame-parameter it 'parent-frame) t)
		     (delete-frame) (frame-list))))

;; Basic Utility Key binds
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

(defun thetasigma--delete-frame-or-kill-emacs ()
  "Delete the selected frame, kill Emacs if only one frame is present.

   This function is courtesy of user Drew from Emacs StackExchange."
  (interactive)
  (condition-case nil (delete-frame)
    (error (save-buffers-kill-terminal))))

(defun thetasigma--quit-window ()
  "Intelligent quit window.

   If more than one window is open, close window on quit.  If only one
   window is open and buffer is read only, kill buffer.  If the
   current frame is a child frame, delete it"
  (interactive)
  (cond ((> (length (window-list)) 1)
	 (quit-window t (get-buffer-window (buffer-name))))
	((eq (length (window-list)) 1)
	 (quit-window t))
	((eq (frame-parameter nil 'parent-frame) t)
	 (delete-frame))))

(use-package frame
  :ensure nil
  :bind
  ([remap save-buffers-kill-terminal] . thetasigma--delete-frame-or-kill-emacs)

  :custom
  (default-frame-alist (append (list
				'(fullscreen . fullboth)
				'(vertical-scroll-bars . nil)
				'(internal-border-width . 24)
				'(left-fringe    . 1)
				'(right-fringe   . 1)
				'(tool-bar-lines . 0)
				'(menu-bar-lines . 0)
				'(right-divider-width . 0))))
  (frame-title-format nil)
  (fill-column 80)

  (window-divider-mode nil)
  :config
  (if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

  ;; No toolbar
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  ;; No menu bar
  (if (display-graphic-p) (menu-bar-mode t) (menu-bar-mode -1)))

(use-package window
  :ensure nil
  :bind
  ([remap quit-window] . thetasigma--quit-window)
  :custom
  (window-min-height 1))


(provide 'thetasigma-defaults)
;;; thetasigma-defaults.el ends here.

;; Local Variables:
;; eval: (set-fill-column 70)
;; End:
