;;; thetasigma-enlight.el --- startup-screen customiztion -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: ((enlight "0.3"))

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

;; Creates the default startup screen.  Can also be called interactively with (enlight-open)

;;; Code:

(cond ((package-installed-p 'grid)
       (require 'grid))
      ((not (package-installed-p 'grid))
       (package-vc-install "https://github.com/ichernyshovvv/grid.el")
       (require 'grid)))

(cond ((package-installed-p 'enlight)
       (require 'enlight)
       (require 'enlight-menu))
      ((not (package-installed-p 'enlight))
       (package-vc-install 'enlight)
       (require 'enlight)
       (require 'enlight-menu)))

(defface ts-enlight-logo-face
  '((t (:inherit 'bold :height 2.5)))
  "Face used in the enlight start screen logo."
  :group 'ts-enlight)

(defvar ts-enlight--logo
  (propertize "Θ Σ - Emacs" 'face 'ts-enlight-logo-face)
  "Θ Σ - Emacs Logo.")

(defvar ts-enlight--conf-menu
  '(("Θ Σ Emacs - CONFIG"
     ("Open Directory" (dired thetasigma-dir) "t")
     ("Open Main File" (find-file (concat thetasigma-dir "thetasigma.el")) "c"))
    ("QUICK CONFIG"
     ("Open Θ Σ Org mode conf" (find-file (concat thetasigma-dir "thetasigma-writing.el")) "o")
     ("Open User init.el conf" (find-file user-init-file) "i"))
    )
  "Sub-menu for use in englight screen to access configs.")

(defvar ts-enlight--help
  '(("Files and Folders"
     ("Open a file" (call-interactively 'find-file) "C-x C-f")
     ("Open a directory" (call-interactively 'dired) "C-x d"))
    ("Text Editing Basics"
     ("Cut" nil "C-w")
     ("Copy" nil "M-w")
     ("Paste" nil "C-y")
     ("Clipboard" nil "M-y")
     ("Set mark" nil "C-SPC")
     ("Rect mark" nil "C-x SPC"))
    ("Emergency"
     ("Quit/Save" (call-interactively 'thetasigma--keyboard-quit) "C-g")
     ("Kill Frame" (call-interactively 'thetasigma-frame-delete-frame-or-kill-emacs "C-x C-c"))
     ("Help for help" (call-interactively 'help-for-help) "C-h h")
     ("Θ Σ Emacs keybinds" (call-interactively 'describe-personal-keybindings) "C-h P")
     )
    )
  "Sub-menu for use in englight screen to show common functions.")

(customize-set-value
 'enlight-content
 (grid-get-column `(,(grid-get-box `( :content ,ts-enlight--logo
				      :align 'center
				      :width 80
				      :padding 4))
		    ,(grid-get-row `(,(grid-get-box `( :content ,(enlight-menu ts-enlight--conf-menu)
						       :align 'center
						       :width 80))
				     ,(grid-get-box `( :content ,(enlight-menu ts-enlight--help)
						       :align 'left
						       :width 80))))
		    ( :content
		      ,(propertize "\n\nΘ Σ Emacs - https://github.com/skye-repos/thetasigma-emacs\n"
				   'face 'shadow :height 0.8)
		      :align 'left
		      :width 160))))


(setopt initial-buffer-choice #'enlight)

(provide 'thetasigma-enlight)
;;; thetasigma-enlight.el ends here.
