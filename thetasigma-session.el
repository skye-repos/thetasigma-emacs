;; -*- lexical-binding: t -*-
;; ---------------------------------------------------------------------
;; GNU Emacs / Θ Σ - Emacs for Memacs
;; Copyright (C) 2024 - Θ Σ developers 
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

;; Backups
(unless (file-exists-p "~/.emacs.d/saves")
  (make-directory "~/.emacs.d/saves"))

(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-old-versions 6)
(setq kept-new-versions 9)

;; Save minibuffer history
(setq savehist-additional-variables
      '(kill-ring
        command-history
	    query-replace-history     
	    minibuffer-history        
	    file-name-history)
      )
(setq history-length 150)
(setq kill-ring-max 50)
(put 'command-history            'history-length 10)
(put 'query-replace-history      'history-length 10)
(put 'file-name-history          'history-length 30)
(put 'minibuffer-history         'history-length 50)

(savehist-mode t)

;; Remove text properties for kill ring entries
;; See https://emacs.stackexchange.com/questions/4187
(defun unpropertize-kill-ring ()
  (setq kill-ring (mapcar 'substring-no-properties kill-ring)))
(add-hook 'kill-emacs-hook 'unpropertize-kill-ring)

;; Recentf for use with consult-buffer
(setq recentf-max-menu-items 25)
(recentf-mode t)

(provide 'thetasigma-session)
