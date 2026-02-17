;;; thetasigma-dark-low-contrast-theme.el --- Contrast rich dark theme 

;;; Commentary:
;; High Contrast Dark Mode

;;; Code:
(require-theme 'thetasigma-theme)

(defcustom thetasigma-dark-low-contrast-colors
  '((foreground . "#EED9EE")
    (background . "#14121C")
    (bg-overlay . "#24142C")
    (bold . "#F9FFF9")
    (bold-distant . "#141414")
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
  "Low(er) Contrast Dark mode colors")

(deftheme thetasigma-dark-low-contrast "Contrast rich & WCAG compliant dark theme"
		  :family 'thetasigma
		  :kind 'dark
		  :background-mode 'dark)

(thetasigma-theme-define 'thetasigma-dark-low-contrast thetasigma-dark-low-contrast-colors)

(provide-theme 'thetasigma-dark-low-contrast)
(run-hooks 'thetasigma-theme-after-load-hook)
(provide 'thetasigma-dark-low-contrast-theme)
;;; thetasigma-dark-low-contrast-theme.el ends here
;; Local Variables:
;; eval: (rainbow-mode) (set-fill-column 70)
;; End:
