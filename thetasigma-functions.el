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
(defvar thetasigma-dir "~/.emacs.d/thetasigma-emacs/"
  "Directory that Θ Σ - Emacs was cloned into.")

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

(provide 'thetasigma-defaults)
;;; thetasigma-defaults.el ends here.

;; Local Variables:
;; eval: (set-fill-column 70)
;; End:
