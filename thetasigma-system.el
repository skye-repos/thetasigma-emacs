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

   These are collected over time and tweaked to my needs.
   Customization of some of these to be added."
  (interactive)
  ;; Mac special keys to C-M-S language
  (setq ns-use-native-fullscreen t
        mac-command-modifier 'meta
        mac-option-modifier 'super)
  ;; Setting some external programs either use Homebrew or
  ;; Macports. You can also put whatever binary - for instance eza
  ;; instead of gnu ls.
        ;; Mac Ports Gls
  (cond ((and (file-exists-p "/opt/local/bin/gls")
			  (file-executable-p "/opt/local/bin/gls"))
		 (setq insert-directory-program "/opt/local/bin/gls"))
		;; Homebrew Gls
		((and (file-exists-p "/opt/homebrew/bin/gls")
			  (file-executable-p "/opt/homebrew/bin/gls"))
		 (setq insert-directory-program "/opt/homebrew/bin/gls")))

  ;; Fix bug on OSX in term mode & zsh (spurious % after each command)
  (add-hook 'term-mode-hook
			(lambda ()
			  (setq buffer-display-table (make-display-table)))))

;; Mac specific
(when (eq system-type 'darwin)
  (thetasigma-system--mac))

;; Common Stuff
;; GPG SSH setenv
(setenv "SSH_AUTH_SOCK" "~/.gnupg/S.gpg-agent.ssh")

(provide 'thetasigma-system)
;;; thetasigma-system.el ends here

;; Local Variables:
;; eval: (set-fill-column 70) (setq use-hard-newlines t)
;; End:
