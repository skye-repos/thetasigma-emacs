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

;; Stuff
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Use-package
(setq use-package-always-ensure t)

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Use RET to open org-mode links, including those in quick-help.org
(setq org-return-follows-link t)


;; Put customized variables in a separate file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; Text Editing Changes
;(cua-mode t)
(delete-selection-mode t)
(global-visual-line-mode t)
(setq x-underline-at-descent-line t)
(setq org-hide-emphasis-markers t)

;; y/n for  answering yes/no questions
(fset 'yes-or-no-p 'y-or-n-p)

;; Tabs
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Tab.space equivalence
(setq-default tab-width 4)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; Mac specific
(when (eq system-type 'darwin)
  (setq ns-use-native-fullscreen t
        mac-option-key-is-meta nil
        mac-command-key-is-meta t
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
