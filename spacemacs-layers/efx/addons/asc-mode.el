(defface asc-key-source-face
  '((t :foreground "black"
       :background "systemBlueColor"
       ))
  "Face for highlighting '.' in source string."
  :group 'asc-mode
  )

(defface asc-key-eq-face
  '((t :background "sienna1"
       :foreground "black"
       :weight extra-bold
       ))
  "Face for highlighting '='."
  :group 'asc-mode
  )

(defface asc-key-localized-face
  '((t :foreground "black"
       :background "GreenYellow"
       ))
  "Face for highlighting '.' in localized string."
  :group 'asc-mode
  )

(setq asc-highlights
      '(
        ("\\\\[.=\\]" . 'asc-key-source-face)
        ("[.]" . 'asc-key-localized-face)    
        ("=" . 'asc-key-eq-face)
        ))

;;;###autoload
(define-derived-mode asc-mode text-mode "asc"
  "major mode for editing asc files."
  (setq font-lock-defaults '(asc-highlights)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asc\\'" . monkeyc-mode))

(provide 'asc-mode)
