;;; thetasigma-writing.el --- Convenience for writing -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.1"))
;; URL: https://github.com/skye-repos/thetasigma-emacs

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

;; Basic Config for packages including org, latex, and (soon) typst

;; Local Variables:
;; eval: (byte-compile-suppressed-warnings '(not-used))
;; End:

;;; Code:
(use-package cdlatex)

(use-package latex
  :ensure auctex)

(use-package org
  :ensure nil
  :hook
  (org-mode . org-cdlatex-mode)
  (org-mode . org-indent-mode)

  :bind
  (:map org-mode-map
        ("M-<return>" . org-insert-item)
        :prefix-map ctl-z-map
        :prefix "C-z"
        ("C-z a" . org-archive-all-done)
        ("C-z <TAB>" . org-cycle-list-bullet))

  :custom
  (org-directory "~/Documents/Org/")
  (let ((work (concat org-directory "work.org"))
		(personal (concat org-directory "personal.org"))
		(archive (concat org-directory "archive.org")))
	(org-store-new-agenda-file-list (list work personal))
	(org-archive-location (concat archive "::* From %s")))
  
  (org-highlight-latex-and-related '(native latex script))
  (org-export-backends '(latex odt org))

  (org-ellipsis " ▼")
  (org-pretty-entities t)
  (org-hide-emphasis-markers t)

  (org-fontify-todo-headline t)
  (org-fontify-done-headline t)

  (org-src-fontify-natively t)
  (org-src-tab-acts-natively t)
  (org-src-window-setup 'split-window-below)

  (org-todo-keywords '((sequence "TODO(t!)" "|" "IN-PROGRESS(i!)")
					   (sequence "WAITING(w)" "|" "POSTPONED(p)")
					   (sequence "DONE(d!)" "|" "FAILED(f!)")))
  
  (org-enforce-todo-dependencies t)
  (org-enforce-todo-checkbox-dependencies t)

  (org-preview-latex-default-process 'dvisvgm)

  (org-tags-column 80)

  (org-return-follows-link t))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;; Configure Tempel
(use-package tempel
  :custom
  (tempel-trigger-prefix "<")
  :config
  (defun thetasigma-writing--tempel-setup-capf ()
    "Setup templ Capf endpoint."
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  (conf-mode . thetasigma-writing--tempel-setup-capf)
  (prog-mode . thetasigma-writing--tempel-setup-capf)
  (text-mode . thetasigma-writing--tempel-setup-capf))

(use-package tempel-collection)

(provide 'thetasigma-writing)
;;; thetasigma-writing.el ends here
