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
                      :underline 'unspecified :box 'unspecified)
  (set-face-attribute face nil
                      (regexp-opt attr 'words) :inherit styles)
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
                        :underline 'unspecified :box 'unspecified)
    (set-face-attribute face nil
                        (regexp-opt attr) :inherit styles)

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
  (set-face 'highlight '(:foreground thetasigma--background)
                    'thetasigma-face-subtle
                    nil)

  (set-face 'cursor nil
            'region)

  (set-face 'shadow '(:foreground thetasigma--bold)
            'thetasigma-face-font-emphasize)
  (set-face 'success nil
            'thetasigma-face-font-emphasize)
  (set-face 'error '(:foreground thetasigma--standout)
            'thetasigma-face-font-emphasize)
  (set-face 'match nil
            'thetasigma-face-font-bold)

  (set-face 'font-lock-comment-face '(:foreground thetasigma--neutral-0)
            'thetasigma-face-font-light)
  (set-face 'font-lock-doc-face '(:foreground thetasigma--neutral-2)
            'thetasigma-face-font-bold)

  )

(defun thetasigma-theme--vertico ()
  "Bare minimum vertico faces"

  (set-face 'vertico-current nil
            'highlight)

  (set-face 'vertico-multiline nil
            'shadow)

  (set-face 'vertico-group-title nil
            'shadow
            'thetasigma-face-font-emphasize)

  (set-face 'vertico-group-separator '(:strike-through t)
            'shadow)
  )

(defun thetasigma-theme ()
  "Call the fns that set the faces for the modes"
  (thetasigma-theme--minimal)
  (thetasigma-theme--vertico)
  )

(provide-theme 'thetasigma)
(provide 'thetasigma-theme)
