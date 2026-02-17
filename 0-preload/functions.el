;; A better way to use C-g that is a little more context sensitive
(defun thetasigma--keyboard-quit ()
  "Quit current context.

This function is a combination of `keyboard-quit' and `keyboard-escape-quit'
with some custom behavior. Courtesy of u/clemera from Reddit.

- Exit recursive minibufs
- Deactivate region
- If one just exited a mode, do nothing
- Get rid of C-u's
- Quit out of macros"
  (interactive)
  (cond
   ;; If recursive minibufs, exit to top level
   ((> (minibuffer-depth) 0)
	(top-level))
   ;; Avoid adding the region to the window selection.
   ((region-active-p)
	(setq saved-region-selection nil)
	(let (select-active-regions) (deactivate-mark)))
   ;; If the last command was =mode-exited= then return nil.
   ((eq last-command 'mode-exited) nil)
   ;; If you have accidentally added a bunch of C-u's, get rid
   ;; of them. Here, current-prefix-arg returns a non-nil value
   ;; (=> conditional)
   (current-prefix-arg
	(setq current-prefix-arg nil))
   ;; Prevent quit-keyboard being used in a macro. Can be
   ;; annoying. Here, defining-kbd-macro returns a non-nil value
   ;; (=> conditional)
   (defining-kbd-macro
	(message
	 (substitute-command-keys
	  "Quit is ignored during macro definitions.\nUse \\[kmacro-end-macro] if you want to stop macro definition"))
	(cancel-kbd-macro-events))
   ;; Default case
   (t (keyboard-quit))))

(defun thetasigma--delete-frame-or-kill-emacs ()
  "Delete the selected frame, kill Emacs if only one frame is present."
  (interactive)
  (condition-case nil (delete-frame)
    (error (save-buffers-kill-terminal))))

(defun thetasigma--kill-current-buffer-and-window ()
  "Kill current buffer and/or window based on context.  If multiple windows
exist and the buffer is displayed elsewhere, just delete the window.  If it's an
image/PDF, replace the buffer with a Dired/scratch buffer instead of closing the
window. If it's the last window, just kill the buffer."
  (interactive)
  (let* ((win-list (window-list))
         (buf-wins (get-buffer-window-list (current-buffer)))
         (specialp (derived-mode-p 'image-mode 'doc-view-mode 'pdf-view-mode)))
    
    (cond
     ;; Case 1: Only one window exists - just kill the buffer.
     ((= (length win-list) 1)
      (kill-current-buffer))
     ;; Case 2: Special modes (Images/PDFs) - don't delete window, just "pivot"
     (specialp
      (let ((fallback (if default-directory 
                          (dired-noselect default-directory)
                        (get-buffer-create "*scratch*"))))
        (set-window-buffer (selected-window) fallback)))
     ;; Case 3: Multiple windows, buffer is shown in more than one
     ((> (length buf-wins) 1)
      (delete-window))
     ;; Case 4: Multiple windows, this is the only window showing this buffer
     (t
      (kill-current-buffer)
      (delete-window)))))

(defun thetasigma--split-window-dwim ()
  "Split the current window vertically or horizontally based on window's pixel
height and width."
  (interactive)
  (let* ((h (float (window-pixel-height)))
		 (w (float (window-pixel-width)))
		 (r (/ h w)))
	(cond ((> r 1)
		   (split-window-vertically))
		  ((> 1 r)
		   (split-window-horizontally))
		  ((= 1 r)
		   (split-window-sensibly)))))

(defun thetasigma--install-font ()
  "Downloads and installs the 0xProto font for macOS or Linux."
  (interactive)
  (let* ((font-url "https://github.com/0xType/0xProto/releases/latest/download/0xProto-Regular.ttf")
         (font-name "0xProto-Regular.ttf")
         ;; Determine the correct font directory based on the OS
         (dest-dir (cond
                    ((eq system-type 'darwin) 
                     (expand-file-name "~/Library/Fonts/"))
                    ((eq system-type 'gnu/linux) 
                     (expand-file-name "~/.local/share/fonts/"))
                    (t (error "Unsupported OS for this script"))))
         (dest-path (concat dest-dir font-name)))

    ;; 1. Create directory if it doesn't exist
    (unless (file-directory-p dest-dir)
      (make-directory dest-dir t)
      (message "Created directory: %s" dest-dir))

    ;; 2. Download the file
    (if (file-exists-p dest-path)
        (message "Font already exists at %s" dest-path)
      (progn
        (message "Downloading 0xProto to %s..." dest-path)
        (url-copy-file font-url dest-path t)
        
        ;; 3. Refresh font cache (Linux only)
        (when (eq system-type 'gnu/linux)
          (message "Updating Linux font cache...")
          (shell-command "fc-cache -f"))
        
        (message "Font installed successfully! You may need to restart Emacs.")))))
