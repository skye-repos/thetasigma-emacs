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

(require 'thetasigma-functions)
;; Prepare the ctrl-z prefix map
(define-prefix-command 'ctl-z-map)
(keymap-global-set "C-z" ctl-z-map)

;; Buffer related bindings
(thetasigma--keymap-unbind-bind ctl-x-map "k" 'kill-current-buffer)
(thetasigma--global-unbind-bind "<f5>" 'revert-buffer)

;; Help and Documentation
;; Info
(thetasigma--keymap-unbind-bind help-map "C-f" 'Info-goto-emacs-command-node)
(thetasigma--keymap-unbind-bind help-map "C-k" 'Info-goto-emacs-key-command-node)
(thetasigma--keymap-unbind-bind help-map "C-m" 'info-display-manual)
(thetasigma--keymap-unbind-bind help-map "C-s" 'info-lookup-symbol)
(thetasigma--keymap-unbind-bind help-map "C-r" 'info-emacs-manual)
(thetasigma--keymap-unbind-bind help-map "C-b" 'info-emacs-bug)
;; Descriptions
(thetasigma--keymap-unbind-bind help-map "F" 'describe-face)
(thetasigma--keymap-unbind-bind help-map "s" 'describe-symbol)
(thetasigma--keymap-unbind-bind help-map "S" 'describe-syntax)
(thetasigma--keymap-unbind-bind help-map "p" 'describe-package)
(thetasigma--keymap-unbind-bind help-map "P" 'describe-personal-keybindings)
(thetasigma--keymap-unbind-bind help-map "c" 'describe-command)
(thetasigma--keymap-unbind-bind help-map "C" 'describe-coding-system)

;; Minibuffer Quitting
(thetasigma--global-unbind-bind "C-g" 'thetasigma--keyboard-quit-context+)

;; Frame and Window Binds
(thetasigma--keymap-unbind-bind ctl-x-map "C-c" 'thetasigma--delete-frame-or-kill-emacs)

;; Cheatsheet bindings
(thetasigma--global-unbind-bind "M-p" 'thetasigma-quick-help)

;; Expand and Contract region with treesitter
(thetasigma--global-unbind-bind "C-=" 'thetasigma--treesit-mark-bigger-node)

;; Coding and Text Input Binds
(thetasigma--keymap-unbind-bind ctl-z-map "C-<SPC>" 'fixup-whitespace)
(thetasigma--keymap-unbind-bind ctl-z-map "C-e" 'eval-defun)
(thetasigma--keymap-unbind-bind ctl-z-map "C-;" 'comment-dwim)

(provide 'thetasigma-bindings)

