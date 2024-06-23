(defun thetasigma-frame-delete-frame-or-kill-emacs ()
  "Delete the selected frame, kill Emacs if only one frame is present.
This function is courtesy of user Drew from Emacs StackExchange"
  (interactive)
  (condition-case nil (delete-frame) (error (save-buffers-kill-terminal))))

;; A quit window useful for things like dired
(defun thetasigma-frame-quit-window ()
  "If more than one window is open, close window on quit.
If only one window is open and buffer is read only, kill buffer.
If the current frame is a child frame, delete it"
  (interactive)
  (cond ((> (length (window-list)) 1)
	 (quit-window t (get-buffer-window (buffer-name))))
	((eq (length (window-list)) 1)
	 (quit-window t))
	((eq (frame-parameter nil 'parent-frame) t)
	 (delete-frame))))

(use-package frame
  :ensure nil
  :bind
  ([remap save-buffers-kill-terminal] . thetasigma-frame-delete-frame-or-kill-emacs)

  :custom
  (default-frame-alist (append (list
				'(fullscreen . fullboth)
				'(vertical-scroll-bars . nil)
				'(internal-border-width . 24)
				'(left-fringe    . 1)
				'(right-fringe   . 1)
				'(tool-bar-lines . 0)
				'(menu-bar-lines . 0)
				'(right-divider-width . 0))))
  (frame-title-format nil)
  (fill-column 80)

  (window-divider-mode nil)
  :config
  (if (fboundp 'scroll-bar-mode) (set-scroll-bar-mode nil))

  ;; No toolbar
  (if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

  ;; No menu bar
  (if (display-graphic-p) (menu-bar-mode t) (menu-bar-mode -1))
  )

(use-package window
  :ensure nil
  :bind
  ([remap quit-window] . thetasigma-frame-quit-window)
  :custom
  (window-min-height 1)
  )

(provide 'thetasigma-frame)
;;; thetasigma-frame.el ends here
