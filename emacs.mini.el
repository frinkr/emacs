(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux

(setq efx-dir (file-name-directory (file-truename load-file-name)))
(add-to-list 'load-path (concat efx-dir "extra-packages")) ;; include addons directory (not in melpa)

(when is-macos
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin")))
  )

;;;;
;;;;          melpa
;;;;
(when nil
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "127.0.0.1:6667")
          ("https" . "127.0.0.1:6667")))
  )

(defun install-extra-packages()
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

  (package-initialize)

  (setq package-list '(
                       auto-complete
                       cl-lib
                       default-text-scale
                       diminish
                       diff-hl
                       engine-mode
                       helm
                       helm-posframe
                       highlight
                       highlight-parentheses
                       highlight-symbol
                       highlight-thing
                       fill-column-indicator
                       magit
                       p4
                       persp-mode
                       projectile
                       pos-tip
                       undo-tree
                       use-package
                       swiper-helm
                       ))

  (setq theme-list '(
                     color-theme-sanityinc-tomorrow
                     dracula-theme
                     ))
  ;; refresh
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install 
  (dolist (package (append package-list theme-list))
    (unless (package-installed-p package)
      (package-install package)))
  )

(install-extra-packages)
(require 'use-package)


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
(defun fx/setup-general()
  (global-hl-line-mode nil)
  ;;(desktop-save-mode 1)  ;; save session

  ;; line & column number
  (line-number-mode t)
  (column-number-mode t)
  (global-linum-mode 1)

  ;; mouse in terminal
  (xterm-mouse-mode t)
  (tool-bar-mode -1)

  ;; Disable backup and autosave
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  (save-place-mode t)   ;; save cursor position

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

  ;; Tab stop
  (setq-default indent-tabs-mode nil)
  (setq-default tab-width 4)
  (setq indent-line-function 'insert-tab)
  (setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))

  ;; Auto close bracket insertion
  (electric-pair-mode 1)
  
  ;; title bar shows full path
  (setq-default frame-title-format
                (list '((buffer-file-name " %f"
                                          (dired-directory
                                           dired-directory
                                           (revert-buffer-function " %b"
                                                                   ("%b - Dir:  " default-directory)))))))

  ;; font
  (set-face-attribute 'default nil :family "Source Code Pro" :weight 'regular :height 140)

  ;; auto revert
  (global-auto-revert-mode)
  
  ;; Common packages
  (require 'cl)
  (require 'cl-lib)
  (require 'subr-x)
  )


;;;;
;;;;     General keybindings
;;;;
(defun fx/setup-keybindings()
  ;; fast navigation
  (global-set-key (kbd "M-m") 'set-mark-command)
  (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
  (global-set-key (kbd "M-b") (lambda () (interactive) (backward-word)))
  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "M-`") 'set-mark-command)

  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-4") 'p4-edit)
  (global-set-key (kbd "C-x r") 'reload)
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
  (global-set-key (kbd "C-z") 'undo-tree-undo)

  ;; Zoom
  (global-set-key (kbd "<C-wheel-up>") nil)
  (global-set-key (kbd "<C-wheel-down>") nil)

  ;; Comments
  (global-set-key (kbd "C-/") 'comment-region)
  (global-set-key (kbd "C-?") 'uncomment-region)

  ;; Mouse-3
  (global-set-key (kbd "<mouse-3>") 'mouse-popup-menubar-stuff)
  )

;;;;
;;;;             fast-nav-mode-map
;;;;
(defun fx/setup-fast-nav()
  (defvar fx/fast-nav-mode-map
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
    "fx/fast-nav-mode keymap.")

  (define-minor-mode fx/fast-nav-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " fast-nav"
    :keymap fx/fast-nav-mode-map)

  (defun fx/fast-nav/minibuffer-setup-hook ()
    (fx/fast-nav-mode 0))
  (add-hook 'minibuffer-setup-hook 'fx/fast-nav/minibuffer-setup-hook)

  (fx/fast-nav-mode 1)
  )

(defun fx/setup-fill-column-indicator()
  (use-package fill-column-indicator
    :config
    (setq fci-rule-width 1)
    (setq fci-rule-column 90)
    (add-hook 'prog-mode-hook 'fci-mode)
    (add-hook 'text-mode-hook 'fci-mode)
    )
  )


;;;;
;;;;           highlight-thing
;;;;
(defun fx/setup-highlight-thing()
  (use-package highlight
    :preface
    (defun fx/smart-highlight()
      (interactive)
      (when-let ((symbol (thing-at-point 'symbol)))
        (let* ((bounds (bounds-of-thing-at-point 'symbol))
               (begin (car bounds))
               (end (cdr bounds)))
          (dolist (ov (overlays-in begin end))
            (if (overlay-get ov 'hlt-highlight) ;; check if been highlighed
                (hlt-unhighlight-symbol symbol)
              (hlt-highlight-symbol symbol)
              )))))
    :init
    (setq hlt-auto-face-backgrounds '("SpringGreen3" 
                                      "MediumPurple1"
                                      "DarkOrchid4"
                                      "DeepPink"
                                      "DarkOrange"
                                      "OliveDrab4"
                                      "HotPink1"
                                      "IndianRed3"
                                      "RoyalBlue1"
                                      "cyan3"
                                      "RoyalBlue4"))

    :bind(([double-mouse-1] . fx/smart-highlight))
    )
  )

;;;;
;;;;          Highlight-parentheses
;;;;
(defun fx/setup-highlight-parentheses()
  (use-package highlight-parentheses
    :config
    (global-highlight-parentheses-mode t)
    (setq hl-paren-highlight-adjacent t)
    :custom-face (hl-paren-face ((t (:slant italic :weight bold))))
    )
  )

;;;;
;;;;          undo-tree-mode
;;;;
(defun fx/setup-undo-tree()
  (use-package undo-tree
    :preface
    (defun undo-tree-overridden-undo-bindings-p ())   ;; override the function so undo-tree-mode can be force enabled 
    :config
    (global-undo-tree-mode)
    :bind
    (("C-z" . undo-tree-undo)
     ("C-S-z" . undo-tree-redo)
     :map undo-tree-map
     ("C-/" . nil)
     ("C-?" . nil)
     ("C-_" . default-text-scale-decrease)
     )
    )  
  )


;;;;
;;;;        Dired setup
;;;;
(defun fx/setup-dired()
  (use-package dired-x
    :init
    (setq dired-details-hide-link-targets nil)
    :config
    (put 'dired-find-alternate-file 'disabled nil)
    :bind (:map dired-mode-map
                ("RET" . dired-find-alternate-file)
                ("^" . (lambda () (interactive) (find-alternate-file ".."))))
    )
  )


;;;;
;;;;           google
;;;;
(defun fx/setup-google()
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

(defun fx/setup-terminal()
  (when (not (display-graphic-p))

    ;; Mousewheel
    (defun fx/mousewheel-scroll-up (event)
      "Scroll window under mouse up by five lines."
      (interactive "e")
      (let ((current-window (selected-window)))
        (unwind-protect
            (progn
              (select-window (posn-window (event-start event)))
              (scroll-up 5))
          (select-window current-window))))

    (defun fx/mousewheel-scroll-down (event)
      "Scroll window under mouse down by five lines."
      (interactive "e")
      (let ((current-window (selected-window)))
        (unwind-protect
            (progn
              (select-window (posn-window (event-start event)))
              (scroll-down 5))
          (select-window current-window))))

    (global-set-key (kbd "<mouse-5>") 'fx/mousewheel-scroll-up)
    (global-set-key (kbd "<mouse-4>") 'fx/mousewheel-scroll-down)
    )
  )

;;;;
;;;;        persp
;;;;
(defun fx/setup-persp()
  (use-package persp-mode
    :config
    (setq wg-morph-on nil)
    (setq persp-autokill-buffer-on-remove 'kill-weak)     ;; switch off the animation of restoring window configuration
    (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))
)


;;;;
;;;;        helm 
;;;;
(defun fx/setup-helm()
  (use-package helm
    :init
    (setq
     ;;helm-display-function 'helm-display-buffer-in-own-frame
     helm-default-display-buffer-functions '(display-buffer-in-side-window)
     helm-display-buffer-reuse-frame nil
     helm-use-undecorated-frame-option nil
     helm-echo-input-in-header-line nil ;; hide helm echo
     helm-buffers-fuzzy-matching t
     )
    
    :preface
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
        (+ res bonus)))
    
    :config
    (advice-add 'helm-score-candidate-for-pattern :around #'helm-score-candidate-fix)

    :bind
    (("M-l" . helm-semantic-or-imenu)
     ("C-S-l" . helm-semantic-or-imenu)
     ("C-x C-f" . helm-find-files)
     ("C-x C-r" . helm-recentf)
     ("C-x b" . helm-buffers-list)
     ("M-x" . helm-M-x)
     :map helm-map
          ("<tab>" . helm-execute-persistent-action)
          ("C-z"  . helm-select-action))
    )    ;; end of helm
  
  (use-package swiper-helm
    :init
    (setq swiper-goto-start-of-match t
          swiper-include-line-number-in-search t)
    :bind (("C-s" . swiper)))
  )


;;;;
;;;;       git
;;;;
(defun fx/setup-git()
  (use-package magit)

  (use-package diff-hl
    :config
    (global-diff-hl-mode)
    (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  )
)



;;;;
;;;;        auto-complete
;;;;
(defun fx/ac()
  (use-package pos-tip)  ;; for a nice completion menu and help
  (use-package auto-complete-config
    :config (ac-config-default))
  
  (use-package auto-complete  
    :preface
    (defun auto-complete-mode-maybe ()
      "Overwrite auto-complete-mode-maybe which by defaults turns autocomplete only on for buffers listed in ac-modes."
      (unless (minibufferp (current-buffer))
        (auto-complete-mode 1)))
    
    :init
    (setq-default ac-sources '(
                               ac-source-abbrev
                               ac-source-dictionary
                               ac-source-words-in-same-mode-buffers
                               ac-source-words-in-buffer
                               ac-source-files-in-current-dir      
                               ))
        (setq ac-auto-start t
              ac-use-quick-help nil
              ac-dwim t
              ac-use-fuzzy t
              ac-ignore-case t)
    :config
    (global-auto-complete-mode t)    
    ;; Face
    (set-face-background 'popup-scroll-bar-foreground-face "red3")

    ;; Trigger key
    (if (display-graphic-p)
        (global-set-key [(control ?/)] 'auto-complete)
      (ac-set-trigger-key "TAB"))

    (ac-linum-workaround)
    )
  ;; ac conflicts with fill-column-indicator
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/21#issuecomment-6959718
  )


;;;;
;;;;            flyspell
;;;;
(defun fx/setup-flyspell()
  (use-package flyspell
    :config
    (dolist (hook '(text-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))
    (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
      (add-hook hook (lambda () (flyspell-mode -1))))

    (add-hook 'c++-mode-hook  (lambda () (flyspell-prog-mode)))
    )
  )

;;;;
;;;;
;;;;
(defun fx/setup-diminish()

  (require 'diminish)

  (eval-after-load "flycheck" '(diminish 'flycheck-mode nil))
  (eval-after-load "auto-complete" '(diminish 'auto-complete-mode nil))
  (eval-after-load "p4" '(diminish 'p4-mode nil))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode nil))
  (eval-after-load "Undo-Tree" '(diminish 'undo-tree-mode nil))
  (eval-after-load "hl-p" '(diminish 'highlight-parentheses-mode nil))

  (diminish 'persp nil)
  (diminish 'highlight-parentheses-mode nil)
  (diminish 'fx/fast-nav-mode nil)
  
  )

(defun fx/setup-miniconfig-packages()
  (use-package p4
    :init (setenv "P4CONFIG" "p4.config"))

  ;;(require 'monkeyc-mode)
  ;;(require 'asc-mode)

  (use-package default-text-scale
    :bind(("C-+" . default-text-scale-increase)
          ("C-_" . default-text-scale-decrease)))
)


;;;;
;;;;        C++
;;;;
(defun fx/setup-c++()
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
  (setq fx/cc-base-style
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

  (setq fx/c4-style fx/cc-base-style)
  (setq fx/c2-style fx/cc-base-style)
  (add-to-list 'fx/c4-style '(c-basic-offset . 4))
  (add-to-list 'fx/c2-style '(c-basic-offset . 2))

  ;; my c/c++ hook
  (defun fx/c-c++-mode-hook ()
    (define-key c-mode-base-map "\C-l" 'newline-and-indent)
    (c-add-style "efx2" fx/c2-style t)
    (c-add-style "efx4" fx/c4-style t)
    (goto-address-mode t)  
    )
  (add-hook 'c-mode-common-hook 'fx/c-c++-mode-hook)
  (add-hook 'c++-mode-common-hook 'fx/c-c++-mode-hook)
  )

;;;;
;;;;     indent
;;;;
(defun fx/setup-indent(n)
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


(defun fx/setup-private-addons()
  (require 'monkeyc-mode)
  (require 'asc-mode)
  )

;;;;
;;;;    Esko links
;;;;
(defun fx/setup-esko-links()
  (use-package browse-url)
  (use-package goto-addr
    :init
    (setq esko-project-codes '("DPP" "DPI" "JP"))  ;; Esko project code
    (setq esko-project-regexp
          (string-join (mapcar (lambda (x) (concat x "-[0-9]+"))
                               esko-project-codes) "\\|"))

    :config
    ;; trap goto-address-url-regexp
    (setq goto-address-url-regexp-old goto-address-url-regexp)
    (setq goto-address-url-regexp
          (concat
           goto-address-url-regexp-old
           "\\|" esko-project-regexp))

    ;; trap browse-url-url-at-point
    (defadvice browse-url-url-at-point (after fx/browse-url-url-at-point () activate)
      (if (string-match esko-project-regexp ad-return-value)
          (setq ad-return-value
                (concat "http://jira.esko.com/browse/"
                        (progn (string-match esko-project-regexp ad-return-value)
                               (match-string 0 ad-return-value))))
        )
      )
    )
  )


(defun fx/user-setup()
  (fx/setup-general)
;;  (fx/setup-fill-column-indicator) ;; conflicts with auto-complete
  (fx/setup-fast-nav)
  (fx/setup-highlight-thing)
  (fx/setup-highlight-parentheses)
  (fx/setup-undo-tree)
  (fx/setup-dired)
  (fx/setup-google)
  (fx/setup-terminal)
  (fx/setup-persp)
  (fx/setup-helm)
  (fx/setup-diminish)
  (fx/setup-miniconfig-packages)
  (fx/setup-c++)
  (fx/ac)
  (fx/setup-flyspell)
  (fx/setup-git)
  (fx/setup-indent 4)
  (fx/setup-esko-links)
  (fx/setup-keybindings)  ;; this comes to last to override key bindings
  )

(fx/user-setup)
(load-theme 'sanityinc-tomorrow-blue t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
