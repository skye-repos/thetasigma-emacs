;;; thetasigma-enlight.el --- a startup screen
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
(defgroup ts-enlight nil
  "Group for vars used in the startup-screen customisation.")

(use-package enlight)

(unless (package-installed-p 'grid)
  (package-vc-install '(grid
     :vc-backend Git
     :url "https://github.com/ichernyshovvv/grid.el"
     :branch "master"))
  )

(require 'grid)
(require 'enlight)
(require 'enlight-menu)

(defface ts-enlight-logo-face
  '((t (:inherit 'bold :height 1.5)))
  "Face used in the enlight start screen logo."
  :group 'ts-enlight)

(defcustom ts-enlight--logo
  (propertize "Θ Σ - Emacs" 'face 'ts-enlight-logo-face)
  "Θ Σ - Emacs Logo."
  :group 'ts-enlight
  )

(defcustom ts-enlight--conf-menu
  '(("Θ Σ Emacs - CONFIG"
     ("Open Directory ......." (dired thetasigma-user-dir) "t")
     ("Open Main File ......." (find-file (concat thetasigma-user-dir "thetasigma.el")) "c"))
    ("QUICK CONFIG"
     ("Open Θ Σ Org mode conf" (find-file (concat thetasigma-user-dir "thetasigma-writing.el")) "o")
     ("Open User init.el conf" (find-file user-init-file) "i"))
    )
  "Sub-menu for use in englight screen to access configs."
  :group 'ts-enlight
  )

(defcustom ts-enlight--help
  '(("System File Tree Navigation"
     ("Open a file .........." (call-interactively 'find-file) "C-x C-f")
     ("Open a directory ....." (call-interactively 'dired) "C-x d"))
    ("Text Editing Basics"
     ("Cut Region ..........." nil "C-w")
     ("Copy Region .........." nil "M-w")
     ("Paste/Yank into Buffer" nil "C-y")
     ("Yank Ring ............" nil "M-y")
     ("Set mark ............." nil "C-SPC")
     ("Set rectangular mark  " nil "C-x SPC"))
    ("Emergency"
     ("Quit/Save Me ........." (call-interactively 'thetasigma--keyboard-quit-context+) "C-g")
     ("Exit Emacs/Kill Frame " (call-interactively 'thetasigma--delete-frame-or-kill-emacs "C-x C-c"))
     ("Help for help ........" (call-interactively 'help-for-help) "C-h h")
     ("Θ Σ Emacs keybinds ..." (call-interactively 'describe-personal-keybindings) "C-h P")
     )
    )
  "Sub-menu for use in englight screen to show common functions."
  :group 'ts-enlight
  )

(setq-default enlight-content
              (concat
               (grid-get-box `( :content ,ts-enlight--logo
                                :align 'left
                                :width ,(frame-width)
                                :padding 4
                                :border t))
               (grid-get-box `( :content
                                ,(concat
                                  (grid-get-row `(( :content ,(enlight-menu ts-enlight--conf-menu)
                                                   :align 'left
                                                   :width ,(round (* 0.75 (frame-width)))
                                                   :padding 4)
                                                 ( :content ,(enlight-menu ts-enlight--help)
                                                   :align 'right
                                                   :width ,(- (frame-width) (round (* 0.75 (frame-width))))
                                                   :padding 4)))
                                  (grid-get-box `( :content
                                                   ,(propertize "\n\nΘ Σ Emacs - https://github.com/skye-repos/thetasigma-emacs\n"
                                                                'face 'shadow :height 0.8)
                                                   :align 'center
                                                   :width ,(frame-width)))
                                  )
                                :align 'center
                                :width ,(frame-width)))
               )
              )

              
(setopt initial-buffer-choice #'enlight)

(provide 'thetasigma-enlight)
;;; thetasigma-enlight.el ends here.
