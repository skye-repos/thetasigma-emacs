(leaf typst-ts-mode
  :after '(treesit)
  :vc '( :url "https://github.com/kaction-emacs/typst-ts-mode")
  :config
  (add-to-list 'treesit-language-source-alist '(typst "https://github.com/uben0/tree-sitter-typst"))
  (treesit-install-language-grammar 'typst))
