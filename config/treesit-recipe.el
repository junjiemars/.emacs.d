;; required by `(emacs-home* "config/treesits.el")'
;; from `(nore-emacs)'
(( :lang cpp
   :mode c++-mode
   :url "https://github.com/tree-sitter/tree-sitter-cpp"
   :map c++-ts-mode)
 ( :lang c
   :mode c-mode
   :url "https://github.com/tree-sitter/tree-sitter-c"
   :map c-ts-mode)
 ( :lang c-or-c++
   :mode c-or-c++-mode
   :map c-or-c++-ts-mode)
 ( :lang python
   :mode python-mode
   :url "https://github.com/tree-sitter/tree-sitter-python"
   :map python-ts-mode))
