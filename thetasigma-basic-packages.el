;;; thetasigma-basic-packages.el --- bare minimum for usability
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


;; Minibuffer and goodies
(use-package vertico
  :custom
  (vertico-count 8)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-mode t)
  )

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  (marginalia-mode t)
  )

(use-package vertico-posframe
  :custom (vertico-posframe-mode t))

;; Search and search matching
(use-package consult
  :bind
  (:map global-map
        ("C-s" . consult-line)
        ("C-x b" . consult-buffer))
  )

(use-package orderless
  :custom
  (completion-styles '(orderless))  
  (completion-category-defaults nil)
  (orderless-matching-styles
   '(orderless-literal
     orderless-prefixes
     orderless-initialism
     orderless-regexp)
   )
  )

;; Inline completions
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay nil)
  (corfu-auto-prefix nil)
  (corfu-quit-no-match t)
  (global-corfu-mode t)
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  )

;; Dired
(use-package dired
  :ensure nil
  :config
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-al --group-directories-first --time-style=iso")
  (setq dired-dwim-target t)
  :hook
  (dired-mode . dired-hide-details-mode))

;; Which Key
(use-package which-key
  :custom (which-key-mode t))

;; Icons
(use-package nerd-icons
  :init
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd))
         (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
           (nerd-icons-install-fonts)))
        ((eq system-type 'darwin)
         (unless (file-exists-p "~/Library/Fonts/NFM.ttf")
           (nerd-icons-install-fonts))))
  )

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode)
  )

(use-package nerd-icons-corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

(use-package nerd-icons-completion
  :config
  (nerd-icons-completion-mode)
  )

;; Raibow-mode
(use-package rainbow-mode)

(provide 'thetasigma-basic-packages)
;;; thetasigma-basic-packages.el ends here.
