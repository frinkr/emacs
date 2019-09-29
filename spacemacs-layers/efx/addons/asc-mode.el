(defface asc-mode-source-face
  '((t :foreground "gray30"
       :background "systemBlueColor"
       ))
  "Face for highlighting '.' in source string."
  :group 'asc-mode
  )

(defface asc-mode-eq-face
  '((t :background "sienna1"
       :foreground "gray30"
       :weight extra-bold
       ))
  "Face for highlighting '='."
  :group 'asc-mode
  )

(defface asc-mode-localized-face
  '((t :foreground "gray30"
       :background "GreenYellow"
       ))
  "Face for highlighting '.' in localized string."
  :group 'asc-mode
  )

(setq asc-highlights
      '(
        ("\\\\[.=\\]" . 'asc-mode-source-face)
        ("[.]" . 'asc-mode-localized-face)
        ("=" . 'asc-mode-eq-face)
        ))

;;;###autoload
(define-derived-mode asc-mode text-mode "asc"
  "major mode for editing asc files."
  (setq font-lock-defaults '(asc-highlights)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.asc\\'" . monkeyc-mode))

(provide 'asc-mode)
