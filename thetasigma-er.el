;;; thetasigma-er.el --- expand region with treesit -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: treesit

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

;; Uses Emacs' tree-sitter integration to expand selection semantically

;;; Code:

(require 'treesit)

(defun thetasigma-er--treesit-get-lang ()
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
(defun thetasigma-er--treesit-mark-bigger-node ()
  "Expand region based on semantic units.
Inspired by @skrytebane on Github, modified to use built-in treesit"
  
  (interactive)

  (treesit-parser-create (thetasigma-er--treesit-get-lang))

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

(provide 'thetasigma-er)
;;; thetasigma-er.el ends here
