;;; clean-mode-line.el --- A minimal mode-line inspired by mood-line -*- lexical-binding: t; -*-


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

(defface clean-mode-line-vc-status-neutral
  '((t (:inherit (bold))))
  "Face used for neutral or inactive status indicators in the mode-line."
  :group 'clean-mode-line)

(defface clean-mode-line-vc-status-info
  '((t (:inherit (font-lock-keyword-face))))
  "Face used for generic vc status indicators in the mode-line."
  :group 'clean-mode-line)

(defface clean-mode-line-vc-status-success
  '((t (:inherit (success) :foreground "green")))
  "Face used for success vc status indicators in the mode-line."
  :group 'clean-mode-line)

(defface clean-mode-line-vc-status-warning
  '((t (:inherit (warning))))
  "Face for warning vc status indicators in the mode-line."
  :group 'clean-mode-line)

(defface clean-mode-line-vc-status-error
  '((t (:inherit (error))))
  "Face for error vc stauts indicators in the mode-line."
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
  (let* ((path (buffer-file-path-or-directory))
         (is-short-path (< (length path) (* 0.5 (window-total-width))))
         (fmt (concat (if (and is-short-path (buffer-file-name)) default-directory "") "%b "))
        )
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
   'help-echo "mouse-1: Line Ending"
   'local-map clean-mode-line-eol-map
   ))

(defun clean-mode-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  " 'face 'clean-mode-line-major-mode))

(defun clean-mode-line-segment-encoding ()
  "Displays the encoding of the buffer in the mode-line."
  (propertize
   (concat (let* ((sys (coding-system-plist buffer-file-coding-system))
                  (cat (plist-get sys :category))
                  )
             (cond ((memq cat '(coding-category-undecided coding-category-utf-8))
                    "UTF-8")
                   ((memq cat '(coding-category-utf-8-sig))
                    "UTF-8-SIG")
                   (t (upcase (symbol-name (plist-get sys :name))))))
           "  ")
   
   'face 'clean-mode-line-unimportant
   'help-echo "mouse-1: Buffer Encoding"
   'local-map clean-mode-line-encoding-map
   ))

(defun clean-mode-line-segment-major-mode ()
  "Displays the current major mode in the mode-line."
  (propertize "%m  " 'face 'clean-mode-line-major-mode))

(defun clean-mode-line-segment-misc-info ()
  "Displays the current value of `mode-line-misc-info' in the mode-line."
  (format-mode-line mode-line-misc-info 'clean-mode-line-unimportant)
  )

(defvar-local clean-mode-line--vc-text nil)
(defun clean-mode-line--update-vc-segment (&rest _)
  "Update `clean-mode-line--vc-text' against the current VCS state."
  (setq clean-mode-line--vc-text
        (when (and vc-mode buffer-file-name)
          (let ((backend (vc-backend buffer-file-name))
                (state (vc-state buffer-file-name (vc-backend buffer-file-name))))
            (let ((face 'clean-mode-line-vc-status-neutral))
              (concat (cond ((memq state '(edited added))
                             (setq face 'clean-mode-line-vc-status-info)
                             (propertize "+ " 'face face))
                            ((eq state 'needs-merge)
                             (setq face 'clean-mode-line-vc-status-warning)
                             (propertize "⟷ " 'face face))
                            ((eq state 'needs-update)
                             (setq face 'clean-mode-line-vc-status-warning)
                             (propertize "↑ " 'face face))
                            ((memq state '(removed conflict unregistered))
                             (setq face 'clean-mode-line-vc-status-error)
                             (propertize "✖ " 'face face))
                            (t
                             (setq face 'clean-mode-line-vc-status-success)
                             (propertize "✔ " 'face face)))
                      (propertize (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
                                  'face face
                                  'mouse-face face)
                      "  "))))))


(defun clean-mode-line-segment-vc ()
  "Displays color-coded version control information in the mode-line."
  clean-mode-line--vc-text)

(defconst clean-mode-line-buffer-name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
      (let ((my-file-menu-map (make-sparse-keymap "File")))
        (define-key my-file-menu-map
          [my-file-menu-map-reload]
          '("Reload" . reload)
          )
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
          '("Reveal in Folder" . reveal-in-folder-this-buffer)
          )
        my-file-menu-map)
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

(defconst clean-mode-line-encoding-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line down-mouse-1]
      (let ((my-enc-menu-map (make-sparse-keymap "Buffer Encoding")))
        (define-key my-enc-menu-map
          [my-enc-menu-map-utf16-bom]
          '("UTF-16 (LE with signature)" . (lambda () (interactive) (set-buffer-file-coding-system 'utf-16le-with-signature)))
          )
        (define-key my-enc-menu-map
          [my-enc-menu-map-utf16]
          '("UTF-16" . (lambda () (interactive) (set-buffer-file-coding-system 'utf-16)))
          )
        (define-key my-enc-menu-map
          [my-enc-menu-map-utf8]
          '("UTF-8" . (lambda () (interactive) (set-buffer-file-coding-system 'utf-8)))
          )
        (define-key my-enc-menu-map
          [my-enc-menu-map-utf8-bom]
          '("UTF-8 (with signature)" . (lambda () (interactive) (set-buffer-file-coding-system 'utf-8-with-signature)))
          )
        my-enc-menu-map)
      )
    map))

(defvar clean-mode-line--default-mode-line-format mode-line-format)

(define-minor-mode clean-mode-line-mode
  "Toggle clean-mode-line on or off."
  :group 'clean-mode-line
  :global t
  :lighter nil

  (if clean-mode-line-mode
      (progn
        ;; Setup VC hooks
        (add-hook 'find-file-hook #'clean-mode-line--update-vc-segment)
        (add-hook 'after-save-hook #'clean-mode-line--update-vc-segment)
        (advice-add #'vc-refresh-state :after #'clean-mode-line--update-vc-segment)
        
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
                (:eval (clean-mode-line-segment-vc))
                ;;(:eval (clean-mode-line-segment-misc-info))
                " "))))))
        )
    (progn
      (remove-hook 'find-file-hook #'clean-mode-line--update-vc-segment)
      (remove-hook 'after-save-hook #'clean-mode-line--update-vc-segment)
      (advice-remove #'vc-refresh-state #'clean-mode-line--update-vc-segment)

      (setq-default mode-line-format clean-mode-line--default-mode-line-format)
      )
    )
  )


(provide 'clean-mode-line)

;;; clean-mode-line.el ends here
