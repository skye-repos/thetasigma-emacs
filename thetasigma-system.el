;;; thetasigma-system.el --- OS specifc configs -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))
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

;; Helper Functions
(require 'dash)
(defun thetasigma-system--check-and-setq (files var)
  "Loop over FILES and setq to VAR.

   Files later in the list are given precedence"
  (--map (lambda (a) (unless (file-exists-p a)
					   (setq var a)))
		 files))

;; Mac Specific
(defun thetasigma-system--mac ()
  "Fixes specific to using NS Builds on OSX or MacOS.

   These are collected over time and tweaked to my needs.
   Customization of some of these to be added."
  (interactive)
  (setq ns-use-native-fullscreen t
        mac-command-modifier 'meta
        mac-option-modifier 'super)
  (thetasigma-system--check-and-setq '("/opt/homebrew/bin/gls"
									   "/opt/local/bin/gls")
									 insert-directory-program)

  ;; Fix bug on OSX in term mode & zsh (spurious % after each command)
  (add-hook 'term-mode-hook
			(lambda ()
			  (setq buffer-display-table (make-display-table)))))

;; Mac specific
(when (eq system-type 'darwin)
  (thetasigma-system--mac))

(provide 'thetasigma-system)
;;; thetasigma-system.el ends here

;; Local Variables:
;; eval: (set-fill-column 70)
;; End:
