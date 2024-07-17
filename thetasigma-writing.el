(leaf org
  :hook
  '((org-mode-hook . org-indent-mode))
  :bind
  '(("M-<return>" . org-insert-item)
	("C-z a" . org-archive-all-done)
	("C-z <TAB>" . org-cycle-list-bullet))
  :custom
  '((org-directory . "~/Documents/Org")
	(org-highlight-latex-and-related . '(native latex script))
	(org-export-backends . '(latex odt org))
	(org-ellipsis . " …")
	(org-pretty-entities . t)
	(org-hide-emphasis-markers . t)
	(org-fontify-todo-headline . t)
	(org-fontify-done-headline . t)
	(org-src-fontify-natively . t)
	(org-src-tab-acts-natively . t)
	(org-src-window-setup . 'split-window-below)
	(org-todo-keywords . '((sequence "TODO(t!)" "|" "IN-PROGRESS(i!)")
						   (sequence "WAITING(w)" "|" "POSTPONED(p)")
						   (sequence "DONE(d!)" "|" "FAILED(f!)")))
	(org-enforce-todo-dependencies . t)
	(org-enforce-todo-checkbox-dependencies . t)
	(org-preview-latex-default-process . 'dvisvgm)
	(org-tags-column . 80)
	(org-return-follows-link . t)
	(org-archive-location . "~/Documents/Org/archive.org::* From %s")
	(org-image-actual-width . nil))
  :config
  (org-store-new-agenda-file-list '("~/Documents/Org/work.org" "~/Documents/Org/personal.org")))

(leaf org-modern
  :ensure t
  :hook
  '((org-mode-hook . org-modern-mode)
	(org-agenda-finalize-hook . org-modern-agenda))
  :custom
  '((org-modern-star . 'replace)))

;; Configure Tempel
(leaf tempel
  :ensure t
  :custom
  '((tempel-trigger-prefix . "<"))
  :config
  (defun thetasigma-writing--tempel-setup-capf ()
    "Setup templ Capf endpoint."
    (setq-local completion-at-point-functions
		(cons #'tempel-expand
                      completion-at-point-functions)))
  :hook
  '((conf-mode-hook . thetasigma-writing--tempel-setup-capf)
	(prog-mode-hook . thetasigma-writing--tempel-setup-capf)
	(text-mode-hook . thetasigma-writing--tempel-setup-capf)))

(leaf tempel-collection
  :ensure t)
