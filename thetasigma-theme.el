;; Θ Σ theme - A colourful contrast rich theme.
;; ---------------------------------------------------------------------
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

(deftheme thetasigma
  "The theme used for Θ Σ emacs - Emacs for Memacs. It is built out of the principles from the TransSide theme and uses a similar color palate.")

(require 'thetasigma-faces)

(setq-default thetasigma-font-family "Fira Code")
(setq-default thetasigma-font-size 140)

(thetasigma--faces)

(defun set-face (face &optional attr &rest styles)
  "Cleanup whatever face attributes were present before and build up by inheriting the right styles and optionally specifying other attributes"
  (set-face-attribute face nil
                      :family 'unspecified :foundry 'unspecified
                      :width 'unspecified :slant 'unspecified
                      :weight 'unspecified :height 'unspecified
                      :foreground 'unspecified :background 'unspecified
                      :underline 'unspecified :box 'unspecified
                      :inherit styles)
  )

(defun set-inverse-face (face &optional attr color-face-style &rest styles)
  "Cleanup whatever face attributes were present before and build up by inheriting the right styles and optionally specifying other attributes.
Please ensure that styles have no opinion on colors for fg and bg to avoid conflict."
  (let* ((new-fg (face-background color-face-style))
         (new-bg (face-foreground color-face-style)))
    (set-face-attribute face nil
                        :family 'unspecified :foundry 'unspecified
                        :width 'unspecified :slant 'unspecified
                        :weight 'unspecified :height 'unspecified
                        :foreground new-fg :background new-bg
                        :underline 'unspecified :box 'unspecified
                        :inherit styles)
    )
  )

(defun thetasigma-theme--minimal ()
  "Bare minimum to see something happen"
  (setq frame-background-mode thetasigma-theme-variant)

  (set-foreground-color (face-foreground 'thetasigma-face-default))
  (set-background-color (face-background 'thetasigma-face-default))
  
  (set-face 'default nil
            'thetasigma-face-default
            'thetasigma-face-font-default)
  (set-face 'bold nil
            'thetasigma-face-font-default
            'thetasigma-face-font-bold)
  (set-face 'italic nil
            'thetasigma-face-font-default
            'thetasigma-face-font-emphasize)
  (set-face 'bold-italic nil
            'thetasigma-face-font-default
            'thetasigma-face-font-bold
            'thetasigma-face-font-emphasize)
  (set-face 'region '(:extend t)
            'thetasigma-face-overlay)
  (set-inverse-face 'highlight nil
                    'thetasigma-face-subtle
                    nil)
  (set-face 'cursor nil
            'region)
  )

(thetasigma-theme--minimal)

(provide-theme 'thetasigma)
