;;; thetasigma-theme.el  --- Contrast rich WCAG compliant themes
;; -*- lexical-binding:t -*-

;; Author: Skye
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/skye-repos/thetasigma-theme
;; Keywords: faces, extensions

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
;; This theme was built to use colors from the Trans Flag.  The usage of colors for this theme is categorized by the kind/degree of interaction required from a user.

;; Some are purely static, like keywords in prog-mode, some are static in behavior but interactive in function like regions and points, and some are purely interactive like strings.

;; I have tried to define as few colors as i can and define root faces that common packages inherit from.

;; Lastly, I have tried to ensure that every face is WCAG AAA contrast of 10:1 or better.

;; Currently, three styles are offered - 2 dark themes and one light.  These values can be changed by the user in the =M-x customize= menus

;;; Code:
(defgroup thetasigma nil
  "Customization group for the ΘΣ theme."
  :group 'faces)

(defcustom thetasigma-theme-after-load-hook nil
  "Hook run after a ΘΣ theme variant is loaded."
  :type 'hook
  :group 'thetasigma)

(defmacro thetasigma-theme-define (theme-name theme-alist)
  "A macro to define a theme-variant.
Argument THEME-NAME name of theme.
Argument THEME-ALIST alist of colors to be used in the macro."
  
  `(let* ((get-clr (lambda (key) (alist-get key ,theme-alist)))
		 (class '((class color) (min-colors 89)))
		 (foreground (funcall get-clr 'foreground))
		 (background (funcall get-clr 'background))
		 (bg-overlay (funcall get-clr 'bg-overlay))
		 (bold (funcall get-clr 'bold))
		 (bold-distant (funcall get-clr 'bold-distant))
		 (subtle (funcall get-clr 'subtle))
		 (standout (funcall get-clr 'standout))
		 (overlay (funcall get-clr 'overlay))
		 (interact-0 (funcall get-clr 'interact-0))
		 (interact-1 (funcall get-clr 'interact-1))
		 (interact-2 (funcall get-clr 'interact-2))
		 (static-0 (funcall get-clr 'static-0))
		 (static-1 (funcall get-clr 'static-1))
		 (static-2 (funcall get-clr 'static-2))
		 (neutral-0 (funcall get-clr 'neutral-0))
		 (neutral-1 (funcall get-clr 'neutral-1))
		 (neutral-2 (funcall get-clr 'neutral-2)))

	(custom-theme-set-faces
	 ,theme-name
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

	 `(font-lock-builtin-face ((,class :foreground ,neutral-0 :distant-foreground ,bold-distant)))
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

	 `(mode-line ((,class :background ,background :foreground ,bold :overline ,bold :box (:line-width 4 :color ,background :style nil))))
	 `(mode-line-active ((,class :inherit 'mode-line)))
	 `(mode-line-inactive ((,class :background ,background :foreground ,subtle :overline ,subtle  :box (:line-width 4 :color ,background :style nil))))

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

	 `(consult-file ((,class :foreground ,overlay)))
	 `(consult-bookmark ((,class :foreground ,overlay)))
	 
	 `(vertico-posframe ((,class :background ,bg-overlay)))
	 `(vertico-posframe-border ((,class :background ,neutral-0 :weight light)))
	 `(vertico-posframe-border-2 ((,class :background ,static-0 :weight light)))
	 `(vertico-posframe-border-3 ((,class :background ,interact-0 :weight light)))
	 `(vertico-posframe-border-4 ((,class :background ,neutral-1 :weight light)))

	 `(ansi-color-black ((,class :foreground ,bg-overlay)))
	 `(ansi-color-white ((,class :foreground ,foreground)))
	 `(ansi-color-red ((,class :foreground ,interact-0)))
	 `(ansi-color-magenta ((,class :foreground ,interact-2)))
	 `(ansi-color-blue ((,class :foreground ,static-0)))
	 `(ansi-color-cyan ((,class :foreground ,static-1)))
	 `(ansi-color-green ((,class :foreground ,static-2)))
	 `(ansi-color-yellow ((,class :foreground ,standout)))

	 `(ansi-color-bold ((,class :foreground ,bold :weight bold)))
	 `(ansi-color-faint ((,class :foreground ,subtle)))
	 `(ansi-color-inverse ((,class :foreground ,bg-overlay :background ,standout))))))

(provide 'thetasigma-theme)
;;; thetasigma-theme.el ends here
