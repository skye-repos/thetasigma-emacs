;;; thetasigma-fucntions --- Provide miscellaneous support functions
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

;; Help message
(defun thetasigma-quick-help ()
  "Display Basic Help"
  (interactive)
  (let ((message-log-max nil))
    (message
     (concat
      (propertize "\n" 'face '(:height 0.4))
      " [C-x C-f] Open  [M-w] Copy   [C-w] Cut   [C-s] Search           "
      (propertize "[C-h P] Customized Keybinds [C-g] Cancel" 'face 'bold)
      "\n"
      " [C-x C-s] Save  [C-y] Paste  [C-/] Undo  [M-x] Command          "
      (propertize "[C-x C-c] Quit" 'face 'bold)
      (propertize "\n " 'face '(:height 0.5))))
    (sit-for 30)))

;; Close frame if not the last, kill emacs else
(defun thetasigma--delete-frame-or-kill-emacs ()
  "Delete the selected frame, kill Emacs if only one frame is present.
This function is courtesy of user Drew from Emacs StackExchange"
  (interactive)
  (condition-case nil (delete-frame) (error (save-buffers-kill-terminal))))

;; A better way to use C-g that is a little more context sensitive
(defun thetasigma--keyboard-quit-context+ ()
  "Quit current context.

   This function is a combination of `keyboard-quit' and
   `keyboard-escape-quit' with some parts omitted and some custom
   behavior added.

   This function is courtesy of u/clemera from reddit"
  (interactive)
  (cond
   ;; Avoid adding the region to the window selection.
   ((region-active-p) 
    (setq saved-region-selection nil)
    (let (select-active-regions) (deactivate-mark)))
   ;; If the last command was =mode-exited= then return nil.
   ((eq last-command 'mode-exited) nil)
   ;; If you have accidenally added a bunch of C-u's, get rid of them. Here, current-prefix-arg returns a non-nil value (=> conditional)
   (current-prefix-arg nil)
   ;; Prevent quit-keyboard being used in a macro. Can be annoying. Here, defining-kbd-macro returns a non-nil value (=> conditional)
   (defining-kbd-macro (message (substitute-command-keys
                                 "Quit is ignored during macro defintion, use \\[kmacro-end-macro] if you want to stop macro definition"))
                       (cancel-kbd-macro-events))
   ;; Kill all child-frames
   ((frame-parent)
    (delete-frame))
   ((not (frame-parent))
    (dolist (frame (frame-list))
      (if (frame-parameter frame 'parent-frame)
          (delete-frame frame t)))
    )
   ;; Default case
   (t (keyboard-quit))
   )
  )

;; A quit window useful for things like dired
(defun thetasigma--quit-window ()
  "If more than one window is open, close window on quit.
If the current frame is a child frame, delete it"
  (interactive)
  (if (> (length (window-list)) 1) (delete-window) (quit-window))
  (if (eq (frame-parameter nil 'parent-frame) t) (delete-frame))
  )

;; Unbind and bind keybinds
(defun thetasigma--global-unbind-bind (key fun)
  "Takes in KEY (a string representing a key sequence) and a function FUN and unbinds all global bindings previously present and binds it to the provided funcion."
  (keymap-global-unset key)
  (keymap-global-set key fun)
  )

(defun thetasigma--keymap-unbind-bind (map key fun)
  "Takes in a KEY (a string representing a key sequence), MAP (representing a keymap), and FUN (a function name) and unbinds all bindings in that keymap previously present and binds it to the provided funcion."
  (keymap-unset map key)
  (keymap-set map key fun)
  )

(defun thetasigma--treesit-get-lang ()
  "Get prog lang of buffer from file name."
  (interactive)
  (cond ((string= "emacs-lisp"
                  (intern (car (split-string (symbol-name (cdr (assoc 'major-mode (buffer-local-variables (current-buffer))))) "-mode")))
                  )
         (intern-soft "elisp")
         )
        ((string= (or "sh" "zsh" "fish" "PWSH")
                  (intern (car (split-string (symbol-name (cdr (assoc 'major-mode (buffer-local-variables (current-buffer))))) "-mode")))
                  )
         (intern-soft "bash")
         )
        (intern-soft (car (split-string (symbol-name (cdr (assoc 'major-mode (buffer-local-variables (current-buffer))))) "-mode"))))
  )

;; Change region selection based on treesitter's semantic units
(defun thetasigma--treesit-mark-bigger-node ()
  "Expand region based on semantic units.
Inspired by @skrytebane on Github, modified to use built-in treesit"
  
  (interactive)

  (treesit-parser-create (thetasigma--treesit-get-lang))

  (unless (region-active-p)
    (set-mark (point)))
  (let* ((root (treesit-buffer-root-node (treesit-language-at (point))))
         (node (treesit-node-descendant-for-range root (condition-case nil (region-beginning) (error (point))) (condition-case nil (region-end) (error (point)))))
         (node-start (treesit-node-start node))
         (node-end (treesit-node-end node)))
    ;; Node fits the region exactly. Try its parent node instead.
    (when (and (= (region-beginning) node-start) (= (region-end) node-end))
      (when-let ((node (treesit-node-parent node)))
        (setq node-start (treesit-node-start node)
              node-end (treesit-node-end node))))
    (set-mark node-end)
    (goto-char node-start))
  )

(provide 'thetasigma-functions)
