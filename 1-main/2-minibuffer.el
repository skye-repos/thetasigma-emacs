;; -*- lexical-binding: t; -*-
(leaf icomplete-mode
  :ensure nil
  :hook
  '((after-init-hook . (lambda ()
						 (fido-mode nil))))
  :custom
  '((icomplete-vertical-mode . t)
	(icomplete-show-matches-on-no-input . t)
	(icomplete-hide-common-prefix . nil)
	(icomplete-prospects-height . 5)
	(icomplete-with-completion-tables . t)
	(icomplete-in-buffer . nil)
	(icomplete-scroll . t)))

(leaf marginalia
  :ensure t
  :custom
  '((marginalia-max-relative-age . 0)
	(marginalia-field-width . 50)
	(marginalia-align . 'left)
	(marginalia-mode . t)))
