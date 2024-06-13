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

(require 'thetasigma-functions)

;; Minibuffer and goodies
(use-package vertico
  :bind
  (:map vertico-map
        ([remap keyboard-quit] . thetasigma--keyboard-quit-context+))
  :custom
  (vertico-count 8)
  (vertico-resize t)
  (vertico-cycle nil)
  :init
  (vertico-mode)
  )

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode)
  )

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
  :config
  (setq corfu-auto t)
  (setq corfu-auto-delay 0)
  (setq corfu-auto-prefix 0)
  (setq corfu-quit-no-match t)
  :init
  (global-corfu-mode)
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
  (dired-mode . dired-hide-details-mode)
  :bind
  (:map dired-mode-map
        ("q" . thetasigma--quit-window))
  )

;; Which Key
(use-package which-key
  :config (which-key-mode)
  )

;; Icons
(use-package nerd-icons
  :config
  (cond ((member system-type '(gnu gnu/linux gnu/kfreebsd))
         (unless (file-exists-p "~/.local/share/fonts/NFM.ttf")
           (nerd-icons-install-fonts)))
        ((eq system-type 'darwin)
         (unless (file-exists-p "~/Library/Fonts/NFM.ttf")
           (nerd-icons-install-fonts)))
        )
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

(provide 'thetasigma-basic-packages)
