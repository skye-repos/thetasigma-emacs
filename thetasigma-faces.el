
;; Θ Σ theme - A colourful contrast rich theme.
;; Copyright (C) 2024 - Kaushik S Harith
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
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

(require 'thetasigma-colors)

(defcustom thetasigma-font-family nil
  "Font used in theme"
  :group 'thetasigma-theme)

(defcustom thetasigma-font-size nil
  "Font size used in theme"
  :group 'thetasigma-theme)

(defface thetasigma-face-default nil
  "Default face for the theme"
  :group 'thetasigma-theme)

(defface thetasigma-face-overlay nil
  "Face used when a different background face is needed"
  :group 'thetasigma-theme)

(defface thetasigma-face-subtle nil
  "Face used when a different background face is needed"
  :group 'thetasigma-theme)

(defface thetasigma-face-font-default nil
  "This is a support face that sets font information. Is meant to be inherited with other faces that set the style"
  :group 'thetasigma-theme)

(defface thetasigma-face-font-light nil
  "This is a support face that sets font information. Is meant to be inherited with other faces that set the style"
  :group 'thetasigma-theme)

(defface thetasigma-face-font-bold nil
  "This is a support face that sets font information. Is meant to be inherited with other faces that set the style"
  :group 'thetasigma-theme)

(defface thetasigma-face-font-standout nil
  "This is a support face that sets font information. Is meant to be inherited with other faces that set the style"
  :group 'thetasigma-theme)

(defface thetasigma-face-font-emphasize nil
  "This is a support face that sets font information. Is meant to be inherited with other faces that set the style"
  :group 'thetasigma-theme)

(defun thetasigma--faces ()
  "Define the reusable modular faces"
  (set-face-attribute 'thetasigma-face-font-default nil
                      :family thetasigma-font-family
                      :height thetasigma-font-size
                      :weight 'normal
                      :slant 'normal)

  (set-face-attribute 'thetasigma-face-font-light nil
                      :family thetasigma-font-family
                      :height thetasigma-font-size
                      :weight 'light)

  (set-face-attribute 'thetasigma-face-font-bold nil
                      :family thetasigma-font-family
                      :height thetasigma-font-size
                      :weight 'bold)

  (set-face-attribute 'thetasigma-face-font-standout nil
                      :family thetasigma-font-family
                      :height (+ 10 thetasigma-font-size)
                      :weight 'extra-bold
                      :underline t)

  (set-face-attribute 'thetasigma-face-font-emphasize nil
                      :family thetasigma-font-family
                      :height thetasigma-font-size
                      :slant 'italic)

  (set-face-attribute 'thetasigma-face-default nil
                      :foreground thetasigma--foreground
                      :background thetasigma--background)

  (set-face-attribute 'thetasigma-face-overlay nil
                      :foreground thetasigma--bold
                      :background thetasigma--overlay)

  (set-face-attribute 'thetasigma-face-subtle nil
                      :foreground thetasigma--subtle)

  )

(provide 'thetasigma-faces)
