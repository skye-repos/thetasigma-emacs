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

(defvar thetasigma-user-dir
  (concat user-emacs-directory "thetasigma-emacs/")
  "Directory that Θ Σ - Emacs was cloned into")

;;; Code:
(require 'thetasigma-defaults)
(require 'thetasigma-frame)
(require 'thetasigma-window)

;; Load Settings and Binding
(require 'thetasigma-bindings)

;; Load Backups and History
(require 'thetasigma-session)

;; Load Basic Convenience Packages
(require 'thetasigma-basic-packages)

;; Programming niceties
(require 'thetasigma-basic-prog)
(require 'thetasigma-niceties)
(require 'thetasigma-functions)

(require 'thetasigma-writing)

(add-to-list 'custom-theme-load-path thetasigma-user-dir)
(load-theme 'thetasigma t)

(require 'thetasigma-enlight)

(provide 'thetasigma)
;;; thetasigma.el ends here.
