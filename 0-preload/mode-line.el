;;; thetasigma-mode-line.el --- minimal mode-line -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/skye-repos/thetasigma-emacs
;; Keywords: mode-line

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

;; Minor mods to make the mode-line simpler

;;; Code:
(defface thetasigma-mode-line-mod-active nil
  "Mode line face for when the active buffer has been modified.")

(defface thetasigma-mode-line-not-mod-active nil
  "Mode line face for when the active buffer has not been modified.")

(defface thetasigma-mode-line-mod-inactive nil
  "Mode line face for when an inactive buffer has been modified.")

(defface thetasigma-mode-line-not-mod-inactive nil
  "Mode line face for when an inactive buffer has not been modified.")

(let ((static-0 "#84BEFF")
      (static-1 "#84CEEF")
      (static-2 "#84DFCF")
      (neutral-0 "#B9B0FE")
      (neutral-1 "#C8A1FE")
      (neutral-2 "#D790FE"))

  (set-face-attribute 'thetasigma-mode-line-mod-active nil
					  :foreground neutral-2)
  (set-face-attribute 'thetasigma-mode-line-not-mod-active nil
					  :foreground static-2)
  (set-face-attribute 'thetasigma-mode-line-mod-inactive nil
					  :foreground neutral-0)
  (set-face-attribute 'thetasigma-mode-line-not-mod-inactive nil
					  :foreground static-0))

(defvar thetasigma-mode-line-rw
  '(:eval
	(if (mode-line-window-selected-p)
		;; For the Active Modeline
		(if buffer-read-only
			(if (buffer-modified-p)
				(propertize "  " 'face '(:inherit thetasigma-mode-line-mod-active))
			  (propertize "  " 'face '(:inherit thetasigma-mode-line-not-mod-active)))
		  (if (buffer-modified-p)
			  (propertize "  " 'face '(:inherit thetasigma-mode-line-mod-active))
			(propertize "  " 'face '(:inherit thetasigma-mode-line-not-mod-active))))

	  ;; For the inactive modeline
	  (if buffer-read-only
		  (if (buffer-modified-p)
			  (propertize "  " 'face '(:inherit thetasigma-mode-line-mod-inactive))
			(propertize "  " 'face '(:inherit thetasigma-mode-line-not-mod-inactive)))
		(if (buffer-modified-p)
			(propertize "  " 'face '(:inherit thetasigma-mode-line-mod-inactive))
		  (propertize "  " 'face '(:inherit thetasigma-mode-line-not-mod-inactive))))
	  )))

;; Change the mode-line
(setq-default mode-line-format
			  (list
			   " "
			   thetasigma-mode-line-rw
			   " "
			   '(:eval (propertize "%b" 'face '(:weight bold)))
			   " | "
			   '(:eval (propertize "Mode: " 'face '(:slant italic)))
			   '(:eval (propertize (if (car-safe mode-name)
									   (car mode-name)
									   mode-name)
								   'face '(:slant italic)))
			   " | "
			   ;; `(vc-mode vc-mode)
			   '(:eval (propertize (concat "Branch: "
										   (if (equal (car (vc-git-branches)) nil)
											   "None"
											 (car (vc-git-branches))))
								   'face '(:slant italic)))
			   ))
(vc-state (buffer-file-name (current-buffer)))
(provide 'thetasigma-mode-line)
;;; thetasigma-mode-line.el ends here
