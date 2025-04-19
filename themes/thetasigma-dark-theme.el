;;; thetasigma-dark-theme.el --- Colorful contrast -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.1
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/skye-repos/
;; Keywords: Emacs, color-theme, TransSide-theme

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This theme is built from the bones of my old theme - TransSide, using
;; similar principles.  I categorize faces in Emacs based on the
;; kind/degree of interaction required from them.  Some elements like
;; keywords are static, some elements are static in feature but
;; interactive in behavior - like selections, and of course elements
;; that require active interaction like strings and such.

;; I have tried to use as few colors as possible here.  I also try to
;; enforce the WCAG contrast guidelines as much as possible.  The bulk of
;; colors are > than a 10:1 ratio with the background, with some weaker
;; colors for use in subtler contexts.

;;; Code:
(deftheme thetasigma-dark "The theme used for Θ Σ Emacs."
		  :family 'thetasigma
		  :kind 'dark
		  :background-mode 'dark)

(let ((class '((class color) (min-colors 89)))
      (foreground "#FFD9FF")
      (background "#04040C")
      (bg-dim "#04041C")
      (bg-overlay "#4C146C")
      (bold "#F9FFF9")
      (bold-distant "#040004")
      (subtle "#8A87AA")
      (standout "#CCEEB0")
      (overlay "#B0A0EA")
      (interact-0 "#FEA0DF")
      (interact-1 "#FE90DF")
      (interact-2 "#FE90EF")
      (static-0 "#84BEFF")
      (static-1 "#84CEEF")
      (static-2 "#84DFCF")
      (neutral-0 "#B9B0FE")
      (neutral-1 "#C8A1FE")
      (neutral-2 "#D790FE"))

  (custom-theme-set-faces
   'thetasigma-dark
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

   `(link ((,class :foreground ,interact-1 :underline t :weight semi-bold)))

   `(font-lock-builtin-face ((,class :foreground ,neutral-1 :distant-foreground ,bold-distant)))
   `(font-lock-doc-face ((,class :foreground ,neutral-1 :distant-foreground ,bold-distant :weight medium)))
   `(font-lock-comment-face ((,class :foreground ,subtle :distant-foreground ,bold-distant :weight extra-light)))
   `(font-lock-string-face ((,class :foreground ,interact-1 :distant-foreground ,bold-distant :weight medium)))
   `(font-lock-keyword-face ((,class :foreground ,static-0 :distant-foreground ,bold-distant)))
   `(font-lock-constant-face ((,class :foreground ,static-2 :distant-foreground ,bold-distant)))
   `(font-lock-function-name-face ((,class :foreground ,static-2 :distant-foreground ,bold-distant)))
   `(font-lock-variable-name-face ((,class :foreground ,static-0 :distant-foreground ,bold-distant)))
   `(font-lock-type-face ((,class :foreground ,static-1 :distant-foreground ,bold-distant :weight semi-bold)))
   `(font-lock-warning-face ((,class :foreground ,standout :distant-foreground ,bold-distant :weight bold)))
   `(font-lock-property-use-face ((,class :foreground ,static-0 :distant-foreground ,bold-distant :weight normal)))

   `(outline-1 ((,class :foreground ,neutral-0 :weight heavy :height 1.2)))
   `(outline-2 ((,class :foreground ,neutral-1 :weight bold :height 1.1)))
   `(outline-3 ((,class :foreground ,neutral-2 :weight bold :height 1.1)))
   `(outline-4 ((,class :foreground ,static-0 :weight semi-bold :height 1.05)))
   `(outline-5 ((,class :foreground ,interact-0 :weight semi-bold :height 1.05)))
   `(outline-6 ((,class :foreground ,static-1 :weight medium :height 1.05)))
   `(outline-7 ((,class :foreground ,interact-1 :weight medium :height 1.05)))
   `(outline-8 ((,class :foreground ,static-2 :weight medium :height 1.05)))
   
   `(font-latex-doctex-documentation-face ((,class :background ,bg-overlay)))
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

   `(mode-line ((,class :background ,background :foreground ,bold :overline ,bold)))
   `(mode-line-active ((,class :inherit 'mode-line)))
   `(mode-line-inactive ((,class :background ,background :foreground ,subtle :overline ,subtle)))

   `(org-block ((,class :foreground ,bold :weight semi-bold)))
   `(org-ellipsis ((,class :foreground ,subtle)))
   `(org-headline-todo ((,class :foreground ,interact-1)))
   `(org-headline-done ((,class :foreground ,static-1)))

   `(org-level-1 ((,class :inherit outline-1)))
   `(org-level-2 ((,class :inherit outline-2)))
   `(org-level-3 ((,class :inherit outline-3)))
   `(org-level-4 ((,class :inherit outline-4)))
   `(org-level-5 ((,class :inherit outline-5)))
   `(org-level-6 ((,class :inherit outline-6)))
   `(org-level-7 ((,class :inherit outline-7)))
   `(org-level-8 ((,class :inherit outline-8)))

   `(vertico-posframe-border ((,class :background ,neutral-0 :weight light)))
   `(vertico-posframe-border-2 ((,class :background ,static-0 :weight light)))
   `(vertico-posframe-border-3 ((,class :background ,interact-0 :weight light)))
   `(vertico-posframe-border-4 ((,class :background ,neutral-1 :weight light)))
   )
  )
  

(provide-theme 'thetasigma-dark)
(provide 'thetasigma-dark-theme)
;;; thetasigma-dark-theme.el ends here

;; Local Variables:
;; eval: (rainbow-mode) (set-fill-column 70)
;; End:

