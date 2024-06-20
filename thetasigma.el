;;; thetasigma.el --- Emacs for Memacs
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
(defvar thetasigma-dir
  (concat user-emacs-directory "thetasigma-emacs/")
  "Directory that Θ Σ - Emacs was cloned into.")

;; Theme
(add-to-list 'custom-theme-load-path thetasigma-dir)
(load-theme 'thetasigma t)

;; Custom Prefix used in keybinds
(define-prefix-command 'ctl-z-map)
(keymap-global-set "C-z" ctl-z-map)

(require 'thetasigma-defaults)

(require 'thetasigma-frame)


;; Load Settings and Binding
;(require 'thetasigma-bindings)

;; Load Backups and History
(require 'thetasigma-session)

;; Load Basic Convenience Packages
(require 'thetasigma-basic-packages)

(require 'thetasigma-basic-prog)

(use-package thetasigma-er
  :load-path thetasigma-dir
  :bind
  (:map global-map
		("C-=" . thetasigma-er--treesit-mark-bigger-node)))

(require 'thetasigma-writing)
(require 'thetasigma-enlight)

(provide 'thetasigma)
;;; thetasigma.el ends here.
