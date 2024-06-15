
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

(use-package cdlatex)

(use-package latex
  :ensure auctex)

(use-package org
  :hook
  (org-mode . org-cdlatex-mode)
  (org-mode . org-indent-mode)

  :init
  (setq org-highlight-latex-and-related '(native latex script))
  (setq org-export-backends '(latex odt org))

  :bind
  (:map org-mode-map
        ("M-<return>" . org-insert-item)
        :prefix-map ctl-z-map
        :prefix "C-z"
        ("C-z C-e" . thetasigma--org-mark-and-archive)
        ("C-z <up>" . org-cycle-list-bullet))

  :config
  (require 'org-tempo)

  (setq org-directory "~/Documents/Org")

  (defun thetasigma--org-get-path (stringname)
    "Use concat to generate full path."
    (concat (file-name-as-directory org-directory) stringname))

  (setq tasks (thetasigma--org-get-path "Task List.org"))
  (setq archive (thetasigma--org-get-path "archive.org"))

  (setq org-agenda-files (list tasks))
  (setq org-archive-location (concat archive "::* From %s"))

  (setq org-ellipsis " ▼")
  (setq org-src-fontify-natively t)
  (setq org-src-tab-acts-natively t)

  (setq org-todo-keywords '((sequence "TODO(t!)" "|" "DOING(i!)" "|" "WAITING(w!)") (sequence "|" "CANCELLED(c)" "|" "DONE(d)")))

  (setq org-enforce-todo-dependencies t)
  (setq org-enforce-todo-checkbox-dependencies t)

  (setq skye/bullets-list '("◉" "●" "○" "⊙"))

  (setq org-src-window-setup 'current-window)

  (defun thetasigma--org-mark-and-archive ()
    "Mark the state of the current subtree as either DONE or CANCELLED and export to my archive.org file"
    (interactive)
    (org-todo (completing-read "Choose a final TODO state" '("DONE" "CANCELLED")))
    (org-archive-subtree))


  (setq org-pretty-entities nil)
  (setq org-preview-latex-default-process 'dvisvgm)

  (setq org-fontify-todo-headline nil)
  (setq org-fontify-done-headline nil)

  (setq org-tags-column 70)
  )

(use-package org-modern
  :config
  (global-org-modern-mode))

(provide 'thetasigma-writing)
