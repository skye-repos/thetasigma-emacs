;;; thetasigma-basic-prog --- Basic packages that help with programming
;;; Commentary:
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

;;; Code:
(use-package treesit
  :ensure nil
  :config
  (add-to-list 'treesit-language-source-alist '(elisp "https://github.com/Wilfred/tree-sitter-elisp"))
  (add-to-list 'treesit-language-source-alist '(c "https://github.com/tree-sitter/tree-sitter-c"))
  (add-to-list 'treesit-language-source-alist '(bash "https://github.com/tree-sitter/tree-sitter-bash"))
  (unless (treesit-language-available-p 'elisp)
    (treesit-install-language-grammar 'elisp))
  (unless (treesit-language-available-p 'c)
    (treesit-install-language-grammar 'c))
  (unless (treesit-language-available-p 'bash)
    (treesit-install-language-grammar 'bash))
  )

(use-package elec-pair
  :ensure nil
  :hook
  (org-mode . (lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t (,electric-pair-inhibit-predicate c))))))

  (org-mode . (lambda ()
                (setq-local electric-pair-pairs (append electric-pair-pairs '((?$ . ?$))))))
  :config
  (electric-pair-mode 1)
  )

(use-package flymake
  :commands flymake-mode
  :config
  (setq flymake-fringe-indicator-position 'left-fringe)
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-proc-compilation-prevents-syntax-check t)
  (setq flymake-wrap-around nil)
  :hook
  (prog-mode . flymake-mode))

(use-package package-lint-flymake
  :after flymake
  :config
  (package-lint-flymake-setup))

(use-package magit)
(provide 'thetasigma-basic-prog)
;;; thetasigma-basic-prog.el ends here
