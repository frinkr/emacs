(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux


;;;;
;;;;           kill all other buffers but current one
;;;;
(defun kill-other-buffers ()
  "Kill all other buffers, but eshell and shell."
  (interactive)
  (mapc 'kill-buffer
        (delq (current-buffer)
              ;; keep eshell and shell
              (cl-remove-if '(lambda (x) (member (buffer-local-value 'major-mode x)
                                                 '(eshell-mode shell-mode)))
                            (buffer-list)))))

;;;;
;;;;          reload current buffer
;;;;
(defun reload ()
  "Reload the buffer w/o prompt."
  (interactive)
  (revert-buffer nil t))


;;;;
;;;;        Behavior Settings
;;;;
(defun efx/setup-general()
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

  ;; title bar shows full path
  (setq-default frame-title-format
                (list '((buffer-file-name " %f"
                                          (dired-directory
                                           dired-directory
                                           (revert-buffer-function " %b"
                                                                   ("%b - Dir:  " default-directory)))))))
  )


;;;;
;;;;     General keybindings
;;;;
(defun efx/setup-keybindings()
  ;; fast navigation
  (global-set-key (kbd "M-m") 'set-mark-command)
  (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
  (global-set-key (kbd "M-b") (lambda () (interactive) (backward-word)))
  (global-set-key (kbd "M-g") 'goto-line)

  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-4") 'p4-edit)
  (global-set-key (kbd "C-x r") 'reload)

  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  (global-set-key (kbd "C-S-l") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-x j") 'kill-other-buffers)

  ;; mac: set control & meta key
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)

  ;; dis evil
  (define-key evil-emacs-state-map (kbd "C-z") nil)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  )

;;;;
;;;;             fast-nav-mode-map
;;;;
(defun efx/setup-fast-nav()
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
  )

;;;;
;;;;           diminish
;;;;
(defun efx/setup-diminish()
  (spacemacs|diminish p4-mode)
  (spacemacs|diminish efx/fast-nav-mode)
  )

(defun efx/config-fill-column-indicator()
  (setq fci-rule-width 1)
  (setq fci-rule-column 90)
  (add-hook 'after-change-major-mode-hook 'fci-mode)
  )

;;;;
;;;;           highlight-thing
;;;;
(defun efx/config-highlight-thing()
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
(defun efx/config-undo-tree()
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
  (add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

  ;; Create my own coding style
  ;; No space before { and function sig indents 4 if argument overflow
  (setq efx/cc-base-style
        '((c-auto-newline                 . nil)
          (c-comment-only-line-offset     . 0)
          (c-echo-syntactic-information-p . nil)
          (c-hungry-delete-key            . t)
          (c-tab-always-indent            . t)
          (c-toggle-hungry-state          . t)
          (c-hanging-braces-alist         . ((substatement-open after)
                                             (brace-list-open)))
          (c-offsets-alist                . ((case-label . 0)
                                             (inline-open . 0)
                                             (substatement-open . 0)
                                             (inlambda . 0) ; no extra indent for lambda
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

  (setq efx/c4-style efx/cc-base-style)
  (setq efx/c2-style efx/cc-base-style)
  (add-to-list 'efx/c4-style '(c-basic-offset . 4))
  (add-to-list 'efx/c2-style '(c-basic-offset . 2))

  ;; my c/c++ hook
  (defun efx/c-c++-mode-hook ()
    ;; Qt keywords
    (setq c-protection-key (concat "\\<\\(public\\|public slot\\|protected"
                                   "\\|protected slot\\|private\\|private slot"
                                   "\\)\\>")
          c-C++-access-key (concat "\\<\\(signals\\|public\\|protected\\|private"
                                   "\\|public slots\\|protected slots\\|private slots"
                                   "\\)\\>[ \t]*:"))

    (progn
      (define-key c-mode-base-map "\C-l" 'newline-and-indent)
      (c-add-style "efx2" efx/c2-style t)
      (c-add-style "efx4" efx/c4-style t)

      ;; Qt colors
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
      )
    )
  (add-hook 'c-mode-common-hook 'efx/c-c++-mode-hook)
  (add-hook 'c++-mode-common-hook 'efx/c-c++-mode-hook)

  )


(defun efx/user-setup()
  (efx/setup-general)
  (efx/setup-keybindings)
  (efx/setup-fast-nav)
  (efx/setup-diminish)
  (efx/setup-c++)
)