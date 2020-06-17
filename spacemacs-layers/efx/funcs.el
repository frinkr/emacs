
(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux

;;;;
;;;;           include addons directory    
;;;;            
(setq efx-dir (file-name-directory (file-truename load-file-name)))
(add-to-list 'load-path (concat efx-dir "addons"))


;;;;
;;;;           clear the recentf 
;;;;

(defun clear-recentf ()
  "Clear the recent file list."
  (interactive)
  (setq recentf-list '())
  )


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
;;;;           copy file path clipboard
;;;;
(defun copy-file-path ()
  "Copy the current file path to the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))


;;;;
;;;;          reload current buffer
;;;;

(defun reload ()
  "Reload the buffer w/o prompt."
  (interactive)
  (revert-buffer nil t))


;;;;
;;;;           set buffer modified
;;;;
(defun touch()
  "Touch the buffer"
  (interactive)
  (shell-command-to-string (concat "chmod +w " (buffer-file-name (current-buffer))))
  (revert-buffer)
  (set-buffer-modified-p t))


;;;;
;;;;          new scratch
;;;;
(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
                          for name = (format "*scratch-%03i*" num)
                          while (get-buffer name)
                          finally return name))
  (text-mode))


;;;;
;;;;            switch buffer
;;;;
(setq switch-buffer-state t)

(defun switch-buffer()
  "Switch next/previous buffer"
  (interactive)
  (if switch-buffer-state
      (progn
        (setq switch-buffer-state nil)
        (next-buffer)
        )
    (progn
      (setq switch-buffer-state t)
      (previous-buffer)
      )
      )
    )


;;;;
;;;;           code
;;;;
(defun code (path)
  "Open file or folder  in vscode"
  (interactive "sOpen in vs-code (default current file): ")
  (if (string= "" path)
      (shell-command (concat "code " buffer-file-name))
    (shell-command (concat "code " path)))
  )


;;;;
;;;;        Behavior Settings
;;;;
(defun efx/setup-general()
  (global-hl-line-mode nil)
  ;;(desktop-save-mode 1)  ;; save session

  ;; no transpaency
  (when nil 
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
    )

  ;; line & column number
  (line-number-mode t)
  (column-number-mode t)

  ;; mouse in terminal
  (xterm-mouse-mode t)
  (tool-bar-mode -1)

  ;; Setup save options (auto and backup) -- still buggy need new Replace func
  ;; Disable backup and autosave
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  (save-place-mode t)   ;; save cursor position

  (setq evil-emacs-state-cursor '(box "red"))
  (setq evil-emacs-state-cursor '(bar "red"))
  (blink-cursor-mode t)

  (setq inhibit-splash-screen t)
  (setq transient-mark-mode t)
  (setq delete-key-deletes-forward t)
  (setq mouse-yank-at-point nil)
  (setq bookmark-save-flag 1)  ;; autosave bookmarks
  (setq-default indent-tabs-mode nil)

  (setq sp-escape-quotes-after-insert nil) ;; don't auto escape in smartparens-mode

  ;; upcase region is anoying
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; Setup time mode
  (setq display-time-day-and-date t)
  (setq display-time-format "%I:%M %p %m/%d")
  (setq display-time-default-load-average nil)
  (display-time-mode t)

  ;; Setup text mode
  ;;(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
  ;;(add-hook 'text-mode-hook '(lambda() (setq fill-column 70)))

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

(defun efx/setup-indent(n)
  ;; java/c/c++
  (setq c-basic-offset n)
  ;; web development
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
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
  (global-set-key (kbd "M-J") 'helm-cscope-find-global-definition)
  (global-set-key (kbd "M-K") 'helm-cscope-find-this-symbol)
  (global-set-key (kbd "M-l") 'helm-semantic-or-imenu)
  (global-set-key (kbd "M-`") 'set-mark-command)

  ;; helm-find-files-map
  (when nil
    (progn
      (define-key helm-find-files-map (kbd "C-j") 'helm-find-files-up-one-level)
      (define-key helm-find-files-map (kbd "C-l") 'helm-execute-persistent-action)
      (define-key helm-find-files-map (kbd "C-f") 'helm-ff-RET))
  )

  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-4") 'p4-edit)
  (global-set-key (kbd "C-x r") 'reload)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)

  (global-set-key (kbd "C-x C-r") 'helm-recentf)
  (global-set-key (kbd "C-S-l") 'helm-semantic-or-imenu)
  (global-set-key (kbd "C-S-n") 'ns-next-frame)
  (global-set-key (kbd "C-S-o") 'ns-next-frame)
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "C-`") 'ns-next-frame)
  (global-set-key (kbd "C-1") 'switch-buffer)
  (global-set-key (kbd "C-x j") 'kill-other-buffers)
  (global-set-key (kbd "C-t") 'new-scratch)
  ;;(global-set-key (kbd "C-w") 'delete-frame)


  ;; mac: set control & meta key
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)

  ;; dis evil
  (define-key evil-emacs-state-map (kbd "C-z") nil)
  (global-set-key (kbd "C-z") 'undo-tree-undo)

  ;; Zoom
  (global-set-key (kbd "<C-wheel-up>") nil)
  (global-set-key (kbd "<C-wheel-down>") nil)
  (global-set-key (kbd "C-+") 'spacemacs/zoom-frm-in)
  (global-set-key (kbd "C-_") 'spacemacs/zoom-frm-out)
  (define-key undo-tree-map (kbd "C-_") 'spacemacs/zoom-frm-out)

  ;; Comment key-binding
  (define-key undo-tree-map (kbd "C-/") nil)
  (define-key undo-tree-map (kbd "C-?") nil)
  (global-set-key (kbd "C-/") 'comment-region)
  (global-set-key (kbd "C-?") 'uncomment-region)

  ;; Mouse-3
  (global-set-key (kbd "<mouse-3>") 'mouse-set-point)
  ;; multiple-cursors
  ;;(global-unset-key (kbd "M-<down-mouse-1>"))
  ;;(global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
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
      (global-set-key (kbd "<mouse-2>")
                      (lambda ()
                        (interactive)
                        (when (y-or-n-p (format "Close the buffer %s?" (or buffer-file-name (buffer-name))))
                          (kill-buffer (current-buffer)))
                        ))
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
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode)
  )

;;;;
;;;;           org todo
;;;;
(defun efx/setup-org-mode()
  (setq org-todo-keywords-back-up
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "|" "DEFERRED(f@)")))
  
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DEFERRED(f)" "IN-PROGRESS(p)" "|" "DONE(d)" "|" "CANCELED(c)" )))
  (setq org-todo-keyword-faces
        '(("CANCELED" . org-warning)
          ("DEFERRED" . org-warning)
          ("IN-PROGRESS" . org-todo)
          ))
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
;;;;        Dired setup
;;;;
(defun efx/setup-dired()
  ;; show link
  (setq dired-details-hide-link-targets nil)

  ;; reuse the buffer
  (toggle-diredp-find-file-reuse-dir t)
  (put 'dired-find-alternate-file 'disabled nil)

  ;; ^ to go up
  (add-hook 'dired-mode-hook
            (lambda ()
              (define-key dired-mode-map (kbd "^")
                (lambda () (interactive) (find-alternate-file "..")))
                                        ; was dired-up-directory
              ))

  (add-hook 'dired-mode-hook 'efx/dired-mode-hook)
  (defun efx/dired-mode-hook ()
    (local-set-key (kbd "<mouse-2>") 'diredp-mouse-find-file-reuse-dir-buffer))

  )

;;;;
;;;;           eshell
;;;;

(defun efx/setup-eshell()

  ;; max lines
  (setq eshell-buffer-maximum-lines 10000)

  ;;           toggle shell buffer
  ;;
  (defun toggle-shell-buffer (name)
    "Toggle *shell*/*eshell* (`NAME')buffer."
    (interactive)
    (if (string-equal name (buffer-name))
        (previous-buffer)
      (if (get-buffer name)
          (switch-to-buffer name)
        (message (format "buffer %s not exists!" name))
        )))

  (if is-windows
      (define-key global-map "\M-`" (lambda() (interactive) (toggle-shell-buffer "*shell*")))
    (define-key global-map "\M-`" (lambda() (interactive) (toggle-shell-buffer "*eshell*"))))



  ;;           emacs prompt for eshell
  ;;
  (setq eshell-prompt-function
        (lambda ()
          (concat (getenv "USER") "@"
                  (file-name-nondirectory (eshell/pwd))
                  (if (= (user-uid) 0) " # " " $ "))))

  ;; clear the buffer in eshell
  (defun eshell/clear ()
    "clear the eshell buffer."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))



  ;;           clear *shell* buffer
  ;;
  (defun clear-shell ()
    "Clear eshell."
    (interactive)
    (let ((comint-buffer-maximum-size 0))
      (comint-truncate-buffer)))

  (defun clear-shell-hook ()
    "Hook of shell mode."
    (local-set-key "\C-l" 'clear-shell))

  ;;(add-hook 'shell-mode-hook 'clear-shell-hook)

  (defun eshell-clear-buffer ()
    "Clear terminal."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (eshell-send-input)))

  (add-hook 'eshell-mode-hook
            '(lambda()
               (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

  )


;;;;
;;;;           google
;;;;
(defun efx/setup-google()
  (engine-mode t)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defun google (search)
    (interactive (list (engine/get-query (symbol-name 'google))))
    (engine/search-google search)
    )
  )


;;;;
;;;;        mouse scroll in terminal
;;;;

(defun efx/setup-terminal()
  (when (not (display-graphic-p))

    ;; Mousewheel
    (defun efx/mousewheel-scroll-up (event)
      "Scroll window under mouse up by five lines."
      (interactive "e")
      (let ((current-window (selected-window)))
        (unwind-protect
            (progn
              (select-window (posn-window (event-start event)))
              (scroll-up 5))
          (select-window current-window))))

    (defun efx/mousewheel-scroll-down (event)
      "Scroll window under mouse down by five lines."
      (interactive "e")
      (let ((current-window (selected-window)))
        (unwind-protect
            (progn
              (select-window (posn-window (event-start event)))
              (scroll-down 5))
          (select-window current-window))))

    (global-set-key (kbd "<mouse-5>") 'efx/mousewheel-scroll-up)
    (global-set-key (kbd "<mouse-4>") 'efx/mousewheel-scroll-down)
    )
  )



;;;;
;;;;        helm 
;;;;
(defun efx/setup-helm()
  (setq
   ;;helm-display-function 'helm-display-buffer-in-own-frame
   helm-display-buffer-reuse-frame nil
   helm-use-undecorated-frame-option nil
   helm-echo-input-in-header-line nil ;; hide helm echo
   helm-buffers-fuzzy-matching t
   )

  ;;; prefer the candidate with prefix (ignore case)
  (defun helm-score-candidate-fix (orig-fun &rest args)
    (let* ((res (apply orig-fun args))
           (cand (nth 0 args))
           (pattern (nth 1 args))
           (bonus (if (and (stringp cand)
                           (stringp pattern)
                           (string-prefix-p pattern cand t)                   
                           )
                      10000
                    0)))
      ;;(message "cand: %S, pat: %S, bonus %d " cand pattern bonus)
      (+ res bonus)))
  (advice-add 'helm-score-candidate-for-pattern :around #'helm-score-candidate-fix)
  
  )


;;;;
;;;;        auto-complete
;;;;
(defun efx/ac()
  (global-auto-complete-mode t)
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
  (add-to-list 'auto-mode-alist '("\\.ipp\\'" . c++-mode))

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
      )
    )
  (add-hook 'c-mode-common-hook 'efx/c-c++-mode-hook)
  (add-hook 'c++-mode-common-hook 'efx/c-c++-mode-hook)

  )


(defun efx/setup-web()
  (setq-default js2-basic-offset 2
                js-indent-level 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
  )


(defun efx/setup-private-addons()
  (require 'monkeyc-mode)
  (require 'asc-mode)
  )


(defun efx/install-find-file-hook()
  (defun efx/find-file-hook()
    ;; disable useless minor modes
    ;; (persp-mode -1)
    ;; (projectile-mode -1)
    ;; (hs-minor-mode -1)

    ;; remove useless menu items to make the menubar short
    (define-key persp-mode-map [menu-bar Perspectives] nil)
    (define-key projectile-mode-map [menu-bar Projectile] nil)
    (define-key hs-minor-mode-map [menu-bar Hide/Show] nil)
    (define-key semantic-mode-map [menu-bar cedet-menu] nil)


    )
  (add-hook 'find-file-hook 'efx/find-file-hook)
  )


(defun efx/esko-links()
  (require 'goto-addr)
  (require 'browse-url)

  ;; Esko project code
  (setq esko-project-codes '("DPP" "DPI" "JP"))

  (setq esko-project-regexp
        (string-join (mapcar (lambda (x) (concat x "-[0-9]+"))
                             esko-project-codes) "\\|"))

  ;; trap goto-address-url-regexp
  (setq goto-address-url-regexp-old goto-address-url-regexp)
  (setq goto-address-url-regexp
        (concat
         goto-address-url-regexp-old
         "\\|" esko-project-regexp))

  ;; trap browse-url-url-at-point
  (defadvice browse-url-url-at-point (after efx/browse-url-url-at-point () activate)
    (if (string-match esko-project-regexp ad-return-value)
        (setq ad-return-value
              (concat "http://jira.esko.com/browse/"
                      (progn (string-match esko-project-regexp ad-return-value)
                             (match-string 0 ad-return-value))))
      )
    )
)


(defun efx/user-setup()
  (efx/setup-general)
  (efx/setup-indent 4)
  
  (efx/setup-fast-nav)
  (efx/setup-diminish)
  (efx/setup-org-mode)
;;  (efx/setup-dired)
  (efx/setup-terminal)
  (efx/setup-eshell)
  (efx/setup-helm)
  ;;(efx/ac)
  (efx/setup-c++)
  (efx/setup-private-addons)
  (efx/setup-web)
  (efx/install-find-file-hook)
  (efx/esko-links)
  ;; this comes to last to override key bindings
  (efx/setup-keybindings)
)
