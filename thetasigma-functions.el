;; A better way to use C-g that is a little more context sensitive
(defun thetasigma--keyboard-quit ()
  "Quit current context.

   This function is a combination of `keyboard-quit' and
   `keyboard-escape-quit' with some custom behavior.

   Courtesy of u/clemera from Reddit"
  (interactive)
  (progn (cond
	  ;; Avoid adding the region to the window selection.
	  ((region-active-p)
	   (setq saved-region-selection nil)
	   (let (select-active-regions) (deactivate-mark)))
	  ;; If the last command was =mode-exited= then return nil.
	  ((eq last-command 'mode-exited) nil)
	  ;; If you have accidentally added a bunch of C-u's, get rid
	  ;; of them. Here, current-prefix-arg returns a non-nil value
	  ;; (=> conditional)
	  (current-prefix-arg nil)
	  ;; Prevent quit-keyboard being used in a macro. Can be
	  ;; annoying. Here, defining-kbd-macro returns a non-nil value
	  ;; (=> conditional)
	  (defining-kbd-macro
	   (message
	    (substitute-command-keys
	     "Quit is ignored during macro definition, use \\[kmacro-end-macro]
		     if you want to stop macro definition"))
	   (cancel-kbd-macro-events))
	  ;; Default case
	  (t (keyboard-quit)))))

(defun thetasigma--delete-frame-or-kill-emacs ()
  "Delete the selected frame, kill Emacs if only one frame is present.

   This function is courtesy of user Drew from Emacs StackExchange."
  (interactive)
  (condition-case nil (delete-frame)
    (error (save-buffers-kill-terminal))))

(defun thetasigma--quit-window ()
  "Intelligent quit window.

   If more than one window is open, close window on quit.  If only one
   window is open and buffer is read only, kill buffer.  If the
   current frame is a child frame, delete it"
  (interactive)
  (cond ((> (length (window-list)) 1)
	 (quit-window t (get-buffer-window (buffer-name))))
	((eq (length (window-list)) 1)
	 (quit-window t))
	((eq (frame-parameter nil 'parent-frame) t)
	 (delete-frame))))

;; Local Variables:
;; eval: (set-fill-column 70)
;; End:
