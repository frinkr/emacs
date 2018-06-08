(configuration-layer/declare-layers
 '(
   helm
   (colors :variables
           colors-enable-nyan-cat-progress-bar nil)

   ;;themes-megapack
   better-defaults
   emacs-lisp
   markdown

   (auto-completion :variables
                    auto-completion-enable-help-tooltip t
                    auto-completion-enable-sort-by-usage t)

   semantic
   cscope
   (c-c++ :variables
          c-c++-default-mode-for-headers 'c++-mode
          c-c++-enable-clang-support t
    )
   
   
   (spell-checking :variables
                   enable-flyspell-auto-completion t)

   syntax-checking

   version-control
   git
   perforce
   ))
