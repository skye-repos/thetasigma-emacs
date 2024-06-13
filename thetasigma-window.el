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

(use-package window
  :ensure nil
  :init
  (setq display-buffer-alist
        '(;; Right
          ("\\*Messages.*"
           (display-buffer-pop-up-window)
           (window-height . 0.25)
           (window-width . 0.25)
           (side . right)
           (slot . 1))
          ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\)\\*"
           (display-buffer-in-side-window)
           (window-height . 0.25)
           (side . top)
           (slot . 2))
          ;; bottom window
          ("^\\(\\*e?shell\\|vterm\\).*" ;; You don't use eshell. get rid of it
           (display-buffer-in-side-window)
           (window-width . 0.40)
           (side . bottom)
           (slot . 1))
          ;; left side window
          ("\\*Help.*"
           (display-buffer-in-side-window)
           (window-width . 0.35)       ; See the :hook
           (side . left)
           (slot . 0))
          ;; right window
          ("\\*Faces\\*"
           (display-buffer-in-side-window)
           (window-width . 0.35)
           (side . right)
           (slot . 0)
           (window-parameters . ((mode-line-format . (" " mode-line-buffer-identification)))))
          ("\\*Custom.*"
           (display-buffer-in-side-window)
           (window-width . 0.35)
           (side . right)
           (slot . 1))
          ))

  (setq window-combination-resize t)
  (setq even-window-sizes 'height-only)
  (setq window-sides-vertical nil)
  
  :hook ((help-mode . visual-line-mode)
         (custom-mode . visual-line-mode))
  )

(display-buffer-pop-up-frame "*scratch*" thetasigma--child-frame-params-alist)

(setq thetasigma--child-frame-params-alist
  '((visibility . t)
    (auto-raise . t)
    (icon-type . nil)
    (wait-for-wm . nil)
    (inhibit-double-buffering . t)
    (skip-taskbar . t)
    (no-focus-on-map . t)
    (no-accept-focus . t)
    (undecorated . t)
    (ns-transparent-titlebar . t)
    (child-frame-border-width . 10)
    (top-edge . 0.3)
    (right-edge . 0.1)
    (height . 0.1)
    (width . 0.3)
    )
  )
