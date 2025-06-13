;; Minibuffer and goodies
(leaf vertico
  :ensure t
  :custom
  '((vertico-count . 7)
	(vertico-resize . t)
	(vertico-cycle . t)
	(vertico-mode . t))
  :config
  (with-eval-after-load 'jinx
    (add-to-list 'vertico-multiform-categories
				 '(jinx grid (vertico-grid-annotate . 20)))
	(vertico-multiform-mode 1)))

(leaf marginalia
  :ensure t
  :custom
  '((marginalia-max-relative-age . 0)
	(marginalia-field-width . 50)
	(marginalia-align . 'left)
	(marginalia-mode . t)))

(leaf vertico-posframe
  :ensure t
  :custom '((vertico-posframe-mode . t)
			(vertico-posframe-border-width . 1)
			(vertico-posframe-min-width . 75)
			(vertico-posframe-min-height . 5)))

;; (leaf nova
;;   :ensure t
;;   :vc '(:url "https://github.com/thisisran/nova.git")
;;   :require nova-vertico nova-corfu nova-corfu-popupinfo nova-eldoc 
;;   :custom
;;   '((nova-vertico-mode . t)
;; 	(nova-corfu-mode . t)
;; 	(nova-corfu-popupinfo-mode . t)
;; 	(nova-eldoc-mode . t)))
