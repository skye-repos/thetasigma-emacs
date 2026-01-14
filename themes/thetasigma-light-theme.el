(require-theme 'thetasigma-theme)

(deftheme thetasigma-light "Contrast rich & WCAG compliant light theme"
		  :family 'thetasigma
		  :kind 'light
		  :background-mode 'light)

(defcustom thetasigma-light-colors
  '((foreground . "#04040C")
    (background . "#FFF9FE")
    (bg-overlay . "#FCE4FC")
	(bold . "#44346C")
    (bold-distant . "#F9FFF9")
    (subtle . "#4A476A")
    (standout . "#3C7E60")
    (overlay . "#40307A")
    (interact-0 . "#6E103F")
    (interact-1 . "#7E203F")
	(interact-2 . "#8E203F")
    (static-0 . "#242EBF")
    (static-1 . "#243EAF")
    (static-2 . "#244E7F")
    (neutral-0 . "#49308E")
    (neutral-1 . "#58307E")
    (neutral-2 . "#67206E"))
  "Light mode colors")

(thetasigma-theme-define 'thetasigma-light thetasigma-light-colors)

(provide-theme 'thetasigma-light)
(provide 'thetasigma-light-theme)
;; Local Variables:
;; eval: (rainbow-mode) (set-fill-column 70)
;; End:
