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

(let ((class '((class color) (min-colors 89)))
      (foreground "#FFE7FE") ;; WCAG 14
      (background "#1C141C") ;;
      (bold "#F9FFF9") ;; WCAG 16
      (subtle "#93C0A9") ;; WCAG 8 
      (standout "#FFEEB0") ;; WCAG 14 
      (overlay "#B0A0EA") ;; WCAG 7
      (interact-0 "#FEB1FE") ;; WCAG 10
      (interact-1 "#FFC0FF") ;; WCAG 11
      (interact-2 "#FFD0EF") ;; WCAG 12
      (static-0 "#84D5FF") ;; WCAG 10 
      (static-1 "#98DEFF") ;; WCAG 11 
      (static-2 "#8AECFF") ;; WCAG 12 
      (neutral-0 "#D9C0FF") ;; WCAG 10
      (neutral-1 "#D8B1FE") ;; WCAG 9
      (neutral-2 "#D7A0FE") ;; WCAG 8
      )

  (custom-theme-set-faces
   'thetasigma
   `(default ((,class :background ,background :foreground ,foreground)))
   `(bold ((,class :foreground ,bold :weight bold)))
   `(italic ((,class :slant oblique)))
   `(bold-italic ((,class :weight bold :slant oblique)))
   
   `(cursor ((,class :background ,overlay)))

   `(shadow ((,class :foreground ,subtle :weight light)))
   `(success ((,class :weight semi-bold :slant oblique)))
   `(error ((,class :foreground ,standout :weight bold)))

   `(region ((,class :background ,overlay :foreground ,background)))
   `(highlight ((,class :background ,neutral-2 :foreground ,background)))

   `(font-lock-builtin-face ((,class :foreground ,neutral-0)))
   `(font-lock-doc-face ((,class :foreground ,neutral-1 :weight medium)))
   `(font-lock-comment-face ((,class :foreground ,neutral-2 :weight light)))
   `(font-lock-string-face ((,class :foreground ,interact-1 :weight medium)))
   `(font-lock-keyword-face ((,class :foreground ,static-0)))
   `(font-lock-function-name-face ((,class :foreground ,bold :weight extra-bold)))
   `(font-lock-constant-face ((,class :foreground ,static-2)))
   `(font-lock-type-face ((,class :foreground ,static-1 :weight semi-bold)))
   `(font-lock-warning-face ((,class :foreground ,standout :weight bold)))

   `(mode-line-active ((,class :background ,overlay :foreground ,background :weight normal)))
   `(mode-line-inactive ((,class :background ,subtle :foreground ,background :weight light)))

   `(org-block ((,class :foreground ,bold :weight semi-bold)))
   )
  )
  

(provide-theme 'thetasigma)
(provide 'thetasigma-theme)
  

