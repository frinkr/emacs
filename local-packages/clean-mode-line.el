;;; clean-mode-line.el --- A minimal mode-line inspired by doom-modeline -*- lexical-binding: t; -*-


(defun clean-mode-line--format (left right)
  "Return a string of `window-width' length containing LEFT and RIGHT, aligned respectively."
  (let ((reserve (length right)))
    (concat left
            " "
            (propertize " "
                        'display `((space :align-to (- right ,reserve))))
            right)))

(defgroup clean-mode-line nil
  "A minimal mode-line configuration inspired by doom-modeline."
  :group 'clean-mode-line)

(defface clean-mode-line-major-mode
  '((t (:inherit (bold))))
  "Face used for major mode indicator in the mode-line."
  :group 'clean-mode-line)

(defface clean-mode-line-unimportant
  '((t (:inherit (shadow))))
  "Face used for less important mode-line elements."
  :group 'clean-mode-line)

(defface clean-mode-line-modified
  '((t (:inherit (error))))
  "Face used for the 'modified' indicator symbol in the mode-line."
  :group 'clean-mode-line)

(defface clean-mode-line-buffer-name
  '((t (:inherit (mode-line-buffer-id))))
  "Face used for major mode indicator in the mode-line."
  :group 'clean-mode-line)

(defun clean-mode-line-segment-modified()
  "Displays the modification/read-only indicator in the mode-line"
  (if (buffer-file-name)
      (if (buffer-modified-p)
          (propertize "● " 'face 'clean-mode-line-modified)
        (if (and buffer-read-only (buffer-file-name))
            (propertize "🔒 " 'face 'clean-mode-line-unimportant)
          "  "))
    "  ")
  )

(defun clean-mode-line-segment-buffer-name()
  "Display full path of current buffer in the mode-line"
  (let ((fmt (concat (if (buffer-file-name) default-directory "") "%b "))
        (path (buffer-file-path-or-directory)))
    (propertize fmt
                'face 'clean-mode-line-buffer-name
                'help-echo path
                'local-map clean-mode-line-buffer-name-map
                )
    ))

(defun clean-mode-line-segment-position()
  "Display line:column and percentage in the mode-line"
  (list (propertize "(%l:%c) %p%% "
                    'face 'clean-mode-line-unimportant
                    'local-map mode-line-column-line-number-mode-map))
  )

(defun clean-mode-line-segment-eol()
  "Displays the EOL style of the current buffer in the mode-line."
  (propertize
   (pcase (coding-system-eol-type buffer-file-coding-system)
     (0 "LF  ")
     (1 "CRLF  ")
     (2 "CR  "))
   'face 'clean-mode-line-unimportant
   'help-echo "mouse-1: EOL menu"
   'local-map clean-mode-line-eol-map
   ))

(defun clean-mode-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  " 'face 'clean-mode-line-major-mode))

(defun clean-mode-line-segment-encoding ()
  "Displays the encoding and EOL style of the buffer in the mode-line."

  (concat (let ((sys (coding-system-plist buffer-file-coding-system)))
            (cond ((memq (plist-get sys :category) '(coding-category-undecided coding-category-utf-8))
                   "UTF-8")
                  (t (upcase (symbol-name (plist-get sys :name))))))
          "  "))

(defun clean-mode-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  " 'face 'clean-mode-line-major-mode))

(defun clean-mode-line-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (format-mode-line mode-line-misc-info 'clean-mode-line-unimportant)
  )


(defun clean-mode-line-make-buffer-name-menu-map()
  (let ((my-file-menu-map (make-sparse-keymap "My File")))
    (define-key my-file-menu-map
      [my-file-menu-map-code]
      '("Open with VScode" . (lambda () (interactive) (code (buffer-file-path-or-directory))))
      )
    (define-key my-file-menu-map
      [my-file-menu-map-copy]
      '("Copy File Path" . copy-file-path)
      )
    (define-key my-file-menu-map
      [my-file-menu-map-reveal]
      '("Reveal in Finder" . open-in-finder)
      )
    my-file-menu-map)
  )

(defconst clean-mode-line-vc-diff-next
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'diff-hl-next-hunk)
    map)
  )

(defconst clean-mode-line-vc-diff-previous
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1] #'diff-hl-previous-hunk)
    map)
  )


(defconst clean-mode-line-buffer-name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
      (clean-mode-line-make-buffer-name-menu-map)
      )
    map))

(defconst clean-mode-line-eol-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
      (let ((my-eol-menu-map (make-sparse-keymap "Line Ending")))
        (define-key my-eol-menu-map
          [my-eol-menu-map-lf]
          '("LF (unix)" . (lambda () (interactive) (set-buffer-file-coding-system 'unix)))
          )
        (define-key my-eol-menu-map
          [my-eol-menu-map-cr]
          '("CR (mac)" . (lambda () (interactive) (set-buffer-file-coding-system 'mac)))
          )
        (define-key my-eol-menu-map
          [my-eol-menu-map-crlf]
          '("CRLF (dos)" . (lambda () (interactive) (set-buffer-file-coding-system 'dos)))
          )
        my-eol-menu-map)
      )
    map))

(define-minor-mode clean-mode-line-mode
  "Toggle clean-mode-line on or off."
  :group 'clean-mode-line
  :global t
  :lighter nil

  (progn
    
    (setq-default
     mode-line-format
     '((:eval
        (clean-mode-line--format
         ;; Left
         (format-mode-line
          '(" "
            (:eval (clean-mode-line-segment-modified))
            (:eval (clean-mode-line-segment-buffer-name))
            (:eval (clean-mode-line-segment-position))
            )
          )

         ;; Right
         (format-mode-line
          '((:eval (clean-mode-line-segment-eol))
            (:eval (clean-mode-line-segment-encoding))
            (:eval (clean-mode-line-segment-major-mode))
            (:eval (clean-mode-line-segment-misc-info))
            " "))))))
    )
  )


(provide 'clean-mode-line)

;;; clean-mode-line.el ends here
