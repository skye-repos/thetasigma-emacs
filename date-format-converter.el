;;; date-format-converter.el --- Parse and Convert Dates -*- lexical-binding: t -*-

;; Author: Skye
;; Version: 0.0.0
;; Package-Requires: nil
;; Homepage: https://github.com/skye-repos/thetasigma-emacs
;; Keywords: Date, Format, Convert

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

;; I don't know how to use Emacs's date & time parser.  This is me making my life easier.

;;; Code:

(defun normalize-year (year &optional yr-prefix)
  "Normalize the YEAR value.

   By default, a 4 digit year value is left as is, and 1-3 digit year values are
   converted to 2XXX.

   Optionally, if YR-PREFIX is specified - it is added to the beginning of year
   with no modifications.  This is useful if you are dealing with dates in a
   different century."

  (if yr-prefix
	  (string-to-number (concat (number-to-string yr-prefix) (number-to-string year)))
	(cond ((<= 0 year 999)
		   (+ year 2000))
		  ((<= 1000 year)
		   year)
		  ((< year 0)
		   (error "Year less than 0. BC values not supported")))))

(defun validator--day-month (day month year)
  "For use in `validator'.

   Ensures logic of DAY and MONTH in YEAR."
  
  (cond ((member month '(01 03 05 07 08 10 12))
		 (if (<= 1 day 31)
			 (list day month year)
		   (error "Day value %s outside acceptable range for month %s"
				  day month)))
		((member month '(04 06 09 11))
		 (if (<= 1 day 30)
			 (list day month year)
		   (error "Day value %s outside acceptable range for month %s"
				  day month)))
		((and (eq month 02) (eq (% year 4) 0))
		 (if (<= 1 day 29)
			 (list day month year)
		   (error "Day value %s outside acceptable range for month %s in year %s"
				  day month year)))
		((and (eq month 02) (member (% year 4) '(1 2 3)))
		 (if (<= 1 day 28)
			 (list day month year)
		   (error "Day value %s outside acceptable range for month %s in year %s"
				  day month year)))
		((or (< month 0) (> month 12))
		 (error "Invalid month %s" month))))

(defun validator (day month year &optional yr-prefix)
  "Ensure that DAY and MONTH make sense in a given YEAR.

   Year is normalized to a four digit value with a default of 20XX.
   Optionally provide YR-PREFIX for different normalization."

  (if (and (numberp day) (numberp month) (numberp year))
	  (if yr-prefix
		  (validator--day-month day month (normalize-year year yr-prefix))
		(validator--day-month day month (normalize-year year)))
	(error "A non number value was entered for either day, month, or year")))

(defun string-to-date-alist (string delimiter &optional input-format yr-prefix)
  "Split STRING at DELIMITER and return a list with the contents.

   Allowed delimiters include '.', '-', '\\'.

   The default format assumed is day-month-year.  A list is returned of the
   form - (ORDINAL MONTH YEAR).  Optionally, provide INPUT-FORMAT
   as a string from the following options

   1) month-day-year
   2) year-month-day

   Also optionally provide a YR-PREFIX for non 21st century years."
  
  (let* ((delimiter (concat "\\" delimiter))
		 (splits (string-split string delimiter))
		 (yr-prefix (or yr-prefix nil))
		 (t1 (nth 0 splits))
		 (t1 (string-to-number t1))
		 (t2 (nth 1 splits))
		 (t2 (string-to-number t2))
		 (t3 (nth 2 splits))
		 (t3 (string-to-number t3)))
	(cond ((string= input-format "month-day-year")
		   (validator t2 t1 t3 yr-prefix))
		  ((string= input-format "year-month-day")
		   (validator t3 t2 t1 yr-prefix))
		  (t
		   (validator t1 t2 t3 yr-prefix)))))

(provide 'date-format-converter)
;;; date-format-converter.el ends here
