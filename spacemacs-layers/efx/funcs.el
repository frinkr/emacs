(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux


(defun reload ()
  "Reload the buffer w/o prompt."
  (interactive)
  (revert-buffer nil t))

;;;;
;;;;        Behavior Settings
;;;;
(defun efx/general-behavior()
  (global-hl-line-mode nil)
  (desktop-save-mode 1)  ;; save session

  (if is-macos
      (progn
        (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
        (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
        (setq ns-use-proxy-icon nil)
        (setq frame-title-format nil)
        )
    (progn
      ;; transparent
      (set-frame-parameter (selected-frame) 'alpha '(95 90))
      (add-to-list 'default-frame-alist '(alpha 95 90))
      )
    )

  ;; line & column number
  (line-number-mode t)
  (column-number-mode t)

  ;; mouse in terminal
  (xterm-mouse-mode t)
  (tool-bar-mode nil)

  ;; Setup save options (auto and backup) -- still buggy need new Replace func
  ;; Disable backup and autosave
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  (save-place-mode t)   ;; save cursor position

  (setq inhibit-splash-screen t)
  (setq transient-mark-mode t)
  (setq delete-key-deletes-forward t)
  (setq mouse-yank-at-point nil)
  (setq bookmark-save-flag 1)  ;; autosave bookmarks
  (setq-default indent-tabs-mode nil)

  ;; upcase region is anoying
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; Setup time mode
  (setq display-time-day-and-date t)
  (setq display-time-format "%I:%M %p %m/%d")
  (setq display-time-default-load-average nil)
  (display-time-mode t)

  ;; Setup text mode
  (add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
  (add-hook 'text-mode-hook '(lambda() (setq fill-column 70)))

  ;; Tab stop
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))
  )

(efx/general-behavior)


;;;;
;;;;           format title bar to show full path of current file
;;;;
(setq-default frame-title-format
              (list '((buffer-file-name " %f"
                                        (dired-directory
                                         dired-directory
                                         (revert-buffer-function " %b"
                                                                 ("%b - Dir:  " default-directory)))))))



;;;;
;;;;             fast-nav-mode-map
;;;;
(defvar efx/fast-nav-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") (lambda () (interactive) (next-line 5)))
    (define-key map (kbd "M-p") (lambda () (interactive) (previous-line 5)))
    (define-key map (kbd "M-b") (lambda () (interactive) (backward-word)))

    (global-set-key (kbd "C-{") 'tabbar-backward)
    (global-set-key (kbd "C-}") 'tabbar-forward)
    (global-set-key (kbd "C-t") 'new-scratch)

    (define-key global-map [(meta m)] 'set-mark-command)
    (global-set-key (kbd "<mouse-2>") (lambda () (interactive) (kill-buffer (current-buffer))))
    ;;      (global-set-key (kbd "<mouse-3>") 'tabbar-forward)
    ;;      (global-set-key (kbd "<mouse-5>") 'tabbar-backward)

    map)
  "efx/fast-nav-mode keymap.")

(define-minor-mode efx/fast-nav-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " fast-nav"
  :keymap efx/fast-nav-mode-map)

(defun efx/fast-nav/minibuffer-setup-hook ()
  (efx/fast-nav-mode 0))
(add-hook 'minibuffer-setup-hook 'efx/fast-nav/minibuffer-setup-hook)

(efx/fast-nav-mode 1)


;;;;
;;;;           highlight-thing
;;;;
(defun efx/setup-highlight-thing()
  (require 'highlight-symbol)
  (global-set-key [double-mouse-1] 'highlight-symbol)
  ;;  (global-set-key [mouse-1] 'highlight-symbol-remove-all)

  (overlay-lists)

  (setq highlight-symbol-color-pairs
        '(
          ("white" . "SpringGreen3")
          ("white" . "MediumPurple1")
          ("white" . "DarkOrchid4")
          ("white" . "DeepPink")
          ("white" . "DarkOrange")
          ("white" . "OliveDrab4")
          ("white" . "HotPink1")
          ("white" . "IndianRed3")
          ("white" . "RoyalBlue1")
          ("white" . "cyan3")
          ("white" . "RoyalBlue4")
          ))

  (defvar-local highlight-symbol-line-overlay nil "Over used by ")

  (defun highlight-symbol-add-overlay(symbol begin end face)

    (setq highlight-symbol-line-overlay (make-overlay begin end))
    ;;                                             (line-beginning-position)
    ;;                                             (line-end-position)))

    (overlay-put highlight-symbol-line-overlay 'priority -10)
    (overlay-put highlight-symbol-line-overlay 'face face)

    )

  (defun highlight-symbol-post-command-hook ()
    (if highlight-symbol-line-overlay
        (delete-overlay highlight-symbol-line-overlay))
    (let ((begin (line-beginning-position))
          (end (line-end-position))

          )

      ;; find symbols on current line
      ;;(print begin)
      )
    )
  ;;(add-hook 'post-command-hook 'highlight-symbol-post-command-hook nil t)

  (defun highlight-symbol-next-color-pair ()
    "Step to and return next color from the color ring."
    (let ((color (nth highlight-symbol-color-index
                      highlight-symbol-color-pairs)))
      (if color ;; wrap
          (incf highlight-symbol-color-index)
        (setq highlight-symbol-color-index 1
              color (car highlight-symbol-color-pairs)))
      color))

  (defun highlight-symbol-add-overlay-at-point (face)
    "Add overlay at current symbol of current line"
    (let (symbol bounds begin end)
      (setq symbol (thing-at-point 'symbol))
      (setq bounds (bounds-of-thing-at-point 'symbol))
      (setq begin (car bounds))
      (setq end (cdr bounds))

      (highlight-symbol-add-overlay symbol begin end color)
      (print (format "%d, %d" begin end))
      )
    )

  (defun highlight-symbol-add-symbol (symbol &optional color)
    "Override this function for support convenience set foreground and background"
    (unless (highlight-symbol-symbol-highlighted-p symbol)
      (when (equal symbol highlight-symbol)
        (highlight-symbol-mode-remove-temp))
      (let ((color (or color (highlight-symbol-next-color-pair)))
            f-color b-color)
        (unless (facep color)
          (if (consp color)
              (setq f-color (car color)
                    b-color (cdr color))
            (setq f-color highlight-symbol-foreground-color
                  b-color color))
          (setq color `((background-color . ,b-color)
                        (foreground-color . ,f-color))))
        ;; (highlight-symbol-remove-all) ;; remove others
        ;; highlight
        (highlight-symbol-add-symbol-with-face symbol color)
        ;;(push symbol highlight-symbol-list)

        ;;(highlight-symbol-add-overlay-at-point color)

        )))
  )

;;;;
;;;;          undo-tree-mode
;;;;
(defun efx/setup-undo()
  ;; override the function so undo-tree-mode can be
  ;; force enabled
  (defun undo-tree-overridden-undo-bindings-p ())
  (global-undo-tree-mode)

  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  )


;;;;
;;;;        C++
;;;;
(defun efx/setup-c++()
  ;; Setup C mode
  (autoload 'c++-mode  "cc-mode" "C++ Editing Mode" t)
  (autoload 'c-mode    "cc-mode" "C Editing Mode" t)
  (autoload 'c-mode-common-hook "cc-mode" "C Mode Hooks" t)
  (autoload 'c-add-style "cc-mode" "Add coding style" t)


  ;; Associate extensions with modes
  (add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
  ;;(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

  ;; Create my own coding style
  ;; No space before { and function sig indents 4 if argument overflow
  (setq efx/c4-style
        '((c-auto-newline                 . nil)
          (c-basic-offset                 . 4)
          (c-comment-only-line-offset     . 0)
          (c-echo-syntactic-information-p . nil)
          (c-hungry-delete-key            . t)
          (c-tab-always-indent            . t)
          (c-toggle-hungry-state          . t)
          (c-hanging-braces-alist         . ((substatement-open after)
                                             (brace-list-open)))
          (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                             (case-label . 4)
                                             (substatement-open . 0)
                                             (block-open . 0) ; no space before {
                                             (knr-argdecl-intro . -)))
          (c-hanging-colons-alist         . ((member-init-intro before)
                                             (inher-intro)
                                             (case-label after)
                                             (label after)
                                             (access-label after)))
          (c-cleanup-list                 . (scope-operator
                                             empty-defun-braces
                                             defun-close-semi))))

  (setq efx/c2-style
        '((c-auto-newline                 . nil)
          (c-basic-offset                 . 2)
          (c-comment-only-line-offset     . 0)
          (c-echo-syntactic-information-p . nil)
          (c-hungry-delete-key            . t)
          (c-tab-always-indent            . t)
          (c-toggle-hungry-state          . t)
          (c-hanging-braces-alist         . ((substatement-open after)
                                             (brace-list-open)))
          (c-offsets-alist                . ((arglist-close . c-lineup-arglist)
                                             (case-label . 4)
                                             (substatement-open . 0)
                                             (block-open . 0) ; no space before {
                                             (knr-argdecl-intro . -)))
          (c-hanging-colons-alist         . ((member-init-intro before)
                                             (inher-intro)
                                             (case-label after)
                                             (label after)
                                             (access-label after)))
          (c-cleanup-list                 . (scope-operator
                                             empty-defun-braces
                                             defun-close-semi))))


  (defun vlad-cc-style()
    ;;(c-set-style "linux")
    ;;(c-set-offset 'innamespace '0)
    ;;(c-set-offset 'inextern-lang '0)
    (c-set-offset 'inline-open '0)
    ;;(c-set-offset 'label '*)
    ;;(c-set-offset 'case-label '*)
    ;;(c-set-offset 'access-label '/)
    ;;(setq c-basic-offset 4)
    ;;(setq tab-width 4)
    ;;(setq indent-tabs-mode nil)
    )

  (add-hook 'c++-mode-hook 'vlad-cc-style)

  ;; syntax-highlighting for Qt
  ;; (based on work by Arndt Gulbrandsen, Troll Tech)
  (defun jk/c-mode-common-hook ()
    "Set up c-mode and related modes.

 Includes support for Qt code (signal, slots and alikes)."

    ;; base-style
    (c-set-style "stroustrup")
    ;; set auto cr mode
    ;;(c-toggle-auto-hungry-state 0)

    ;; qt keywords and stuff ...
    ;; set up indenting correctly for new qt kewords
    (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                   "\\|protected slot\\|private\\|private slot"
                                   "\\)\\>")
          c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                   "\\|public slots\\|protected slots\\|private slots"
                                   "\\)\\>[ \t]*:"))
    (progn
      ;; modify the colour of slots to match public, private, etc ...
      (font-lock-add-keywords 'c++-mode
                              '(("\\<\\(slots\\|signals\\)\\>" . font-lock-type-face)))
      ;; make new font for rest of qt keywords
      (make-face 'qt-keywords-face)
      (set-face-foreground 'qt-keywords-face "BlueViolet")
      ;; qt keywords
      (font-lock-add-keywords 'c++-mode
                              '(("\\<Q_OBJECT\\>" . 'qt-keywords-face)))
      (font-lock-add-keywords 'c++-mode
                              '(("\\<SIGNAL\\|SLOT\\>" . 'qt-keywords-face)))
      (font-lock-add-keywords 'c++-mode
                              '(("\\<Q[A-Z][A-Za-z]*" . 'qt-keywords-face)))
      ))
  ;;(add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)


  ;; Construct a hook to be called when entering C mode
  (defun efx/c-mode ()
    (progn (define-key c-mode-base-map "\C-l" 'newline-and-indent)
           (c-add-style "fx2" efx/c2-style t)
           (c-add-style "fx4" efx/c4-style t))
    )
  (add-hook 'c-mode-common-hook 'fx/c-mode)
  (add-hook 'c++-mode-common-hook 'fx/c-mode)

  (defadvice c-lineup-arglist (around my activate)
    "Improve indentation of continued C++11 lambda function opened as argument."
    (setq ad-return-value
          (if (and (equal major-mode 'c++-mode)
                   (ignore-errors
                     (save-excursion
                       (goto-char (c-langelem-pos langelem))
                       ;; Detect "[...](" or "[...]{". preceded by "," or "(",
                       ;;   and with unclosed brace.
                       (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
              0                           ; no additional indent
            ad-do-it)))                   ; default behavior

  ;; Setup font-lock syntax coloring package
  (autoload 'font-lock-fontify-buffer "font-lock" "Fontify Buffer" t)
  (condition-case err
      (progn (add-hook 'c-mode-common-hook 'font-lock-fontify-buffer)
             (add-hook 'emacs-lisp-mode-hook 'font-lock-fontify-buffer)
             (global-font-lock t))
    (error (progn
             (message "Could not customize colors, disabling colored fonts.")
             (setq-default font-lock-auto-fontify t))))


  ;; Setup CPerl mode
  (setq cperl-brace-offset -4)
  (setq cperl-indent-level 4)


  ;; Setup Assembler mode
  (defun efx/asm-mode-hook ()
    (progn (setq comment-column 36)
           (setq tab-stop-list '(4 8 12 16 20 24 28 36 40 44 48))))
  (add-hook 'asm-mode-hook 'fx/asm-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.s$" . asm-mode))
  (add-to-list 'auto-mode-alist '("\\.asm$" . asm-mode))

  (autoload 'cpp-font-lock "cpp-mode" "CPP Font Lock mode" t)
  )

(efx/setup-c++)
