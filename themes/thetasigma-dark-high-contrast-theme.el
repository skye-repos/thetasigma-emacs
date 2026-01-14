;;; thetasigma-dark-high-contrast-theme.el --- Dark & High Contrast theme variant

;;; Commentary:
;; 

;;; Code:

(require-theme 'thetasigma-theme)

(deftheme thetasigma-dark-high-contrast "High contrast & WCAG compliant dark theme."
		  :family 'thetasigma
		  :kind 'dark
		  :background-mode 'dark)

(defcustom thetasigma-dark-high-contrast-colors
  '((foreground . "#FFD9FF")
    (background . "#04040C")
    (bg-overlay . "#14142C")
    (bold . "#F9FFF9")
    (bold-distant . "#040004")
    (subtle . "#8A87AA")
    (standout . "#CCEEB0")
    (overlay . "#B0A0EA")
    (interact-0 . "#FEA0DF")
    (interact-1 . "#FE90DF")
    (interact-2 . "#FE90EF")
    (static-0 . "#84BEFF")
    (static-1 . "#84CEEF")
    (static-2 . "#84DFCF")
    (neutral-0 . "#B9B0FE")
    (neutral-1 . "#C8A1FE")
    (neutral-2 . "#D790FE"))
  "High Contrast Dark mode colors.")

(thetasigma-theme-define 'thetasigma-dark-high-contrast thetasigma-dark-high-contrast-colors)

(provide-theme 'thetasigma-dark-high-contrast)
(provide 'thetasigma-dark-high-contrast-theme)
;; Local Variables:
;; eval: (rainbow-mode) (set-fill-column 70)
;; End:

(provide 'thetasigma-dark-high-contrast-theme)

;;; thetasigma-dark-high-contrast-theme.el ends here
