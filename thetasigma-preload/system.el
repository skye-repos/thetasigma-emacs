;;; thetasigma-system.el --- OS specifc configs -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/skye-repos/thetasigma-emacs
;; Keywords: system, config

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

;; Some small tweaks to ease the use between computers (mainly mac and linux)

;;; Code:

;; Mac Specific
(defun thetasigma-system--mac ()
  "Fixes specific to using NS Builds on OSX or MacOS.

   These are collected over time and tweaked to my needs."
  (interactive)
  ;; Stuff specific to the Cocoa build of Emacs.
  (setq ns-use-native-fullscreen t)
  ;; Mac special keys to C-M-S language
  (setq mac-command-modifier 'meta
        mac-option-modifier 'super)

  (unless (package-installed-p 'exec-path-from-shell)
	(package-install 'exec-path-from-shell))
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)
  
  (let* ((gnuls (string-trim-right (shell-command-to-string "which gls") "\n"))
		 ;; (ssh-sock (string-trim-right (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket") "\n"))
		 )
	(if gnuls
		(setq insert-directory-program gnuls)
	  (error "Install GNU ls"))
	;; (if ssh-sock
	;; 	(setenv "SSH_AUTH_SOCK" ssh-sock))
	)

  ;; Fix bug on OSX in term mode & zsh (spurious % after each command)
  (add-hook 'term-mode-hook
			(lambda ()
			  (setq buffer-display-table (make-display-table)))))

(defun thetasigma-system--wsl ()
  "Fixes/tweaks specific to the wsl/linux."
  (interactive)
  (setenv "SSH_AUTH_SOCK" (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))

(cond ((eq system-type 'darwin)
	   (thetasigma-system--mac))
	  ((member system-type '(gnu gnu/linux))
	   (thetasigma-system--wsl)))

(provide 'thetasigma-system)
;;; thetasigma-system.el ends here
