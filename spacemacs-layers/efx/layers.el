(configuration-layer/declare-layers
 '(
   (auto-completion :variables
                    auto-completion-enable-help-tooltip t
                    auto-completion-enable-sort-by-usage t
                    spacemacs-default-company-backends '(company-files company-capf company-semantic)
                    ;;spacemacs-default-company-backends '(company-semantic)
                    )
   autohotkey

   better-defaults

   (colors :variables
           colors-enable-nyan-cat-progress-bar nil)

   (c-c++ :variables
          c-c++-default-mode-for-headers 'c++-mode
          ;; c-c++-enable-clang-support nil ;; doesn't work well for Esko projects
          ) 
   cscope
   csv

   emacs-lisp

   helm
   html

   git

   javascript
   
   markdown

   perforce
   python

   semantic
   (spell-checking :variables
                   enable-flyspell-auto-completion nil)
   syntax-checking

   themes-megapack
   typescript

   version-control
   ))
