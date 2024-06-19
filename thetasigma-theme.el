;;; thetasigma-theme --- A colourful contrast rich theme.
;;; Commentary:
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; ---------------------------------------------------------------------

;;; Code:

(deftheme thetasigma
  "The theme used for Θ Σ Emacs - Emacs for Memacs.  It is built out of the principles from the TransSide theme and uses a similar color palate.")

(let ((class '((class color) (min-colors 89)))
      (foreground "#FFE7FE")
      (background "#04040C")
      (background-weak "#1C041C")
      (bold "#F9FFF9")
      (bold-distant "#040004")
      (subtle "#BA99CA")
      (standout "#FFEEB0")
      (overlay "#B0A0EA")
      (interact-0 "#FEB1FE")
      (interact-1 "#FFC0FF")
      (interact-2 "#FFD0EF")
      (static-0 "#84D5FF")
      (static-1 "#98DEFF")
      (static-2 "#8AECFF")
      (neutral-0 "#D9C0FF")
      (neutral-1 "#D8B1FE")
      (neutral-2 "#D7A0FE")
      )

  (custom-theme-set-faces
   'thetasigma
   `(default ((,class :background ,background :foreground ,foreground :distant-foreground ,bold-distant)))
   `(bold ((,class :foreground ,bold :weight bold)))
   `(italic ((,class :slant oblique)))
   `(bold-italic ((,class :weight bold :slant oblique)))
   
   `(cursor ((,class :background ,overlay)))

   `(shadow ((,class :foreground ,subtle :distant-foreground ,bold-distant :weight light)))
   `(success ((,class :weight semi-bold :distant-foreground ,bold-distant :slant oblique)))
   `(error ((,class :foreground ,standout :distant-foreground ,bold-distant :weight bold)))

   `(region ((,class :background ,overlay :foreground ,background :weight bold :extend t)))
   `(highlight ((,class :background ,neutral-2 :foreground ,background :inverse-video nil)))

   `(font-lock-builtin-face ((,class :foreground ,neutral-0 :distant-foreground ,bold-distant)))
   `(font-lock-doc-face ((,class :foreground ,neutral-1 :distant-foreground ,bold-distant :weight medium)))
   `(font-lock-comment-face ((,class :foreground ,neutral-2 :distant-foreground ,bold-distant :weight extra-light)))
   `(font-lock-string-face ((,class :foreground ,interact-1 :distant-foreground ,bold-distant :weight medium)))
   `(font-lock-keyword-face ((,class :foreground ,static-0 :distant-foreground ,bold-distant)))
   `(font-lock-constant-face ((,class :foreground ,static-2 :distant-foreground ,bold-distant)))
   `(font-lock-function-name-face ((,class :foreground ,static-0 :distant-foreground ,bold-distant)))
   `(font-lock-variable-name-face ((,class :foreground ,interact-1 :distant-foreground ,bold-distant)))
   `(font-lock-type-face ((,class :foreground ,static-1 :distant-foreground ,bold-distant :weight semi-bold)))
   `(font-lock-warning-face ((,class :foreground ,standout :distant-foreground ,bold-distant :weight bold)))

   `(outline-1 ((,class :foreground ,static-2 :weight heavy :height 1.2)))
   `(outline-2 ((,class :foreground ,static-2 :weight bold :height 1.15)))
   `(outline-3 ((,class :foreground ,static-2 :weight bold :height 1.1)))
   `(outline-4 ((,class :foreground ,static-2 :weight semi-bold :height 1.05)))
   `(outline-5 ((,class :foreground ,static-2 :weight semi-bold :height 1.05)))
   `(outline-6 ((,class :foreground ,static-2 :weight medium :height 1.05)))
   `(outline-7 ((,class :foreground ,static-2 :weight medium :height 1.05)))
   `(outline-8 ((,class :foreground ,static-2 :weight medium :height 1.05)))
   
   `(font-latex-doctex-documentation-face ((,class :background ,background-weak)))
   `(font-latex-bold-face ((,class :inherit 'bold)))
   `(font-latex-italic-face ((,class :inherit 'italic)))
   `(font-latex-underline-face ((,class :underline t)))
   `(font-latex-math-face ((,class :foreground ,interact-2 :weight semi-bold)))
   `(font-latex-sedate-face ((,class :inherit 'font-lock-keyword-face)))
   `(font-latex-string-face ((,class :inherit 'font-lock-string-face)))
   `(font-latex-warning-face ((,class :inherit 'error)))
   `(font-latex-verbatim-face ((,class :foreground ,subtle :width semi-expanded)))
   `(font-latex-superscript-face ((,class :height 0.85)))
   `(font-latex-subscript-face ((,class :height 0.85)))
   `(font-latex-script-char-face ((,class :foreground ,subtle :weight light)))
   `(font-latex-sectioning-0-face ((,class :inherit 'outline-1)))
   `(font-latex-sectioning-1-face ((,class :inherit 'outline-2)))
   `(font-latex-sectioning-2-face ((,class :inherit 'outline-3)))
   `(font-latex-sectioning-3-face ((,class :inherit 'outline-4)))
   `(font-latex-sectioning-4-face ((,class :inherit 'outline-5)))
   `(font-latex-sectioning-5-face ((,class :inherit 'outline-6)))

   `(mode-line-active ((,class :background ,overlay :foreground ,background :weight normal)))
   `(mode-line-inactive ((,class :background ,subtle :foreground ,background :weight light)))

   `(org-block ((,class :foreground ,bold :weight semi-bold)))
   )
  )
  

(provide-theme 'thetasigma)
(provide 'thetasigma-theme)

;; Local Variables:
;; eval: (rainbow-mode)
;; End:

;;; thetasigma-theme.el ends here
