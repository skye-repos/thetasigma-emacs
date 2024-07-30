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
(defface thetasigma-mode-line-rw-mod nil
  "Mode line face for when buffer has been modified.")

(defface thetasigma-mode-line-rw-not-mod nil
  "Mode line face for when buffer has not been modified.")

(let ((standout "#FFEEB0")
      (static-2 "#8AECFF"))

  (set-face-attribute 'thetasigma-mode-line-rw-mod nil
					  :foreground standout)
  (set-face-attribute 'thetasigma-mode-line-rw-not-mod nil
					  :foreground static-2))

(defvar thetasigma-mode-line-rw
  '(:eval (if buffer-read-only
			  (if (buffer-modified-p)
				  (propertize "  " 'face '(:inherit thetasigma-mode-line-rw-mod))
				(propertize "  " 'face '(:inherit thetasigma-mode-line-rw-not-mod)))
			(if (buffer-modified-p)
				(propertize "  " 'face '(:inherit thetasigma-mode-line-rw-mod))
			  (propertize "  " 'face '(:inherit thetasigma-mode-line-rw-not-mod))))))

;; Change the mode-line
(setq-default mode-line-format
			  (list
			   " "
			   thetasigma-mode-line-rw
			   " "
			   '(:eval (propertize "%b" 'face '(:weight bold)))
			   '(:eval (propertize "  Mode: " 'face '(:slant italic)))
			   '(:eval (propertize (if (car-safe mode-name)
									   (car mode-name)
									   mode-name)
								   'face '(:slant italic)))))

(provide 'thetasigma-mode-line)
;;; thetasigma-mode-line.el ends here
