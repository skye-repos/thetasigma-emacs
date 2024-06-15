;; ---------------------------------------------------------------------
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

(defgroup thetasigma-theme '()
  "Θ Σ theme group for dark mode colors and faces")

(defcustom thetasigma-theme-variant nil
  "Which version of the theme is being used"
  :group 'thetasigma-theme)

(defcustom thetasigma--foreground nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--background nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--bold nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--subtle nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--standout nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--overlay nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--interact-0 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--interact-1 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--interact-2 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--static-0 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--static-1 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--static-2 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--neutral-0 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--neutral-1 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(defcustom thetasigma--neutral-2 nil
  "To be filled with logic here"
  :group 'thetasigma-theme
  :type 'color)

(setq-default
 thetasigma-theme-variant 'dark
 thetasigma--foreground "#FFE7FE" ;; WCAG 14
 thetasigma--background "#002141" ;;
 thetasigma--bold "#F9FFF9" ;; WCAG 16
 thetasigma--subtle "#93C0A9" ;; WCAG 8 
 thetasigma--standout "#FFEEB0" ;; WCAG 14 
 thetasigma--overlay "#B0A0EA" ;; WCAG 7 can be used as bg when bg is fg
 thetasigma--interact-0 "#FEB1FE" ;; WCAG 10
 thetasigma--interact-1 "#FFC0FF" ;; WCAG 11
 thetasigma--interact-2 "#FFD0EF" ;; WCAG 12
 thetasigma--static-0 "#84D5FF" ;; WCAG 10 
 thetasigma--static-1 "#98DEFF" ;; WCAG 11 
 thetasigma--static-2 "#8AECFF" ;; WCAG 12 
 thetasigma--neutral-0 "#D9C0FF" ;; WCAG 10
 thetasigma--neutral-1 "#D8B1FE" ;; WCAG 9
 thetasigma--neutral-2 "#D7A0FE" ;; WCAG 8
 )

(provide 'thetasigma-colors)
;; Local Variables:
;; eval: (progn (rainbow-mode))
;; End:
