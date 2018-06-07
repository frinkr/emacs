(configuration-layer/declare-layers
 '(
   helm
   auto-completion
   better-defaults
   emacs-lisp
   git
   markdown
   semantic
   cscope
   (c-c++ :variables
          c-c++-default-mode-for-headers 'c++-mode
          c-c++-enable-clang-support t
          )
   ;; org
   spell-checking
   syntax-checking
   version-control
   perforce
   ))
