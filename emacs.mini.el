(setq root-dir (file-name-directory (file-truename load-file-name)))
(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux

(when is-macos
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin:/opt/local/bin"))
  (setq exec-path (append exec-path '("/usr/local/bin" "/opt/local/bin")))
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
  (setq package-archives
        '(("melpa" . "http://elpa.emacs-china.org/melpa/")
          ("org"   . "http://elpa.emacs-china.org/org/")
          ("gnu"   . "http://elpa.emacs-china.org/gnu/")))

  (when is-macos
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")) ;;https://www.reddit.com/r/emacs/comments/cdf48c/failed_to_download_gnu_archive/
  (package-initialize)

  (setq package-list '(cl-lib
                       use-package
                       ))

  ;; refresh
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install 
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

(install-extra-packages)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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
;;;;        Core Settings
;;;;
(when t
  (global-hl-line-mode nil)

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
;;;;             fast-nav-mode-map
;;;;
(when t
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
                      (lambda () (interactive)
                        (when (y-or-n-p (format "Close the buffer %s?" (or buffer-file-name (buffer-name))))
                          (kill-buffer (current-buffer)))
                        ))

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
  
  (use-package diminish 
    :config
    (diminish 'fx/fast-nav-mode nil))
  )

;;;;
;;;;         default-text-scale
;;;;
(use-package default-text-scale
  :bind(("C-+" . default-text-scale-increase)
        ("C-_" . default-text-scale-decrease)))


;;;;
;;;;         fill-column-indicator
;;;;
(use-package fill-column-indicator
  ;;:disabled ;; conflicit with ac
  ;; https://github.com/alpaker/Fill-Column-Indicator/issues/21#issuecomment-6959718
  :config
  (setq fci-rule-width 1)
  (setq fci-rule-column 90)
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'text-mode-hook 'fci-mode)
  )


;;;;
;;;;           highlight-thing
;;;;
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
  (setq hlt-auto-face-foreground "white")
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


;;;;
;;;;          Highlight-parentheses
;;;;

(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (global-highlight-parentheses-mode t)
  :custom
  (hl-paren-highlight-adjacent t)
  :custom-face (hl-paren-face ((t (:slant italic :weight bold))))
  )


;;;;
;;;;          undo-tree-mode
;;;;
(use-package undo-tree
  :diminish undo-tree-mode
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


;;;;
;;;;         comment-dwim-2
;;;;
(use-package comment-dwim-2
  :bind ("C-/" . comment-dwim-2))

;;;;
;;;;        mode-line
;;;;
(use-package mode-line-stats
  :disabled
  :config (mode-line-stats-mode)
  :quelpa (mode-line-stats :fetcher github :repo "Idorobots/mode-line-stats")
)


;;;;
;;;;        Dired setup
;;;;
(use-package dired-x
  :ensure nil
  :init
  (setq dired-details-hide-link-targets nil)
  :config
  (put 'dired-find-alternate-file 'disabled nil)
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  )


;;;;
;;;;           google
;;;;
(use-package engine-mode
  :config
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
(unless (display-graphic-p)
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

;;;;
;;;;        persp
;;;;
(use-package persp-mode
  :disabled
  :diminish persp
  :config
  (setq wg-morph-on nil)
  (setq persp-autokill-buffer-on-remove 'kill-weak)     ;; switch off the animation of restoring window configuration
  (add-hook 'after-init-hook #'(lambda () (persp-mode 1))))



;;;;
;;;;        helm 
;;;;
(use-package helm
  :init
  (setq
   ;;helm-display-function 'helm-display-buffer-in-own-frame
   helm-default-display-buffer-functions '(display-buffer-in-side-window)
   helm-display-buffer-reuse-frame nil
   helm-use-undecorated-frame-option nil
   helm-echo-input-in-header-line nil ;; hide helm echo
   helm-buffers-fuzzy-matching t
   ;;helm-move-to-line-cycle-in-source t
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


;;;;
;;;;         swiper
;;;;
(use-package swiper-helm
  :init
  (setq swiper-goto-start-of-match t
        swiper-include-line-number-in-search t)
  :bind (("C-s" . swiper)))


;;;;
;;;;        projectile
;;;;
(use-package helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'venv-projectile-auto-workon)
  (helm-projectile-on)
  :bind(("C-S-x C-S-f" . helm-projectile-find-file)
        ("C-1" . helm-projectile-find-other-file)
        ("C-S-b" . projectile-compile-project))
  )


;;;;
;;;;       cmake
;;;;
;;(use-package rtags)
(use-package cmake-ide
  :disabled ;; too slow
  ;;:requires rtags
  :init
  (setq cmake-ide-header-search-other-file nil
        cmake-ide-header-search-first-including nil)
  :config
  (cmake-ide-setup)
  )
(use-package cmake-mode)


;;;;
;;;;       git
;;;;
(use-package magit
  :if (not is-windows))
(use-package diff-hl
  :if (not is-windows)
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("C-S-p" . diff-hl-previous-hunk)
         ("C-S-n" . diff-hl-next-hunk))
  )
(use-package ediff
  :ensure nil
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  )


;;;;
;;;;        auto-complete
;;;;
(use-package pos-tip)  ;; for a nice completion menu and help

(use-package auto-complete
  :disabled
  :diminish auto-complete-mode
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
                             ;;ac-source-files-in-current-dir  ;; slow    
                             ))
  (setq ac-auto-start t
        ac-use-quick-help nil
        ac-dwim t
        ac-use-fuzzy t
        ac-ignore-case t)
  :config
  (global-auto-complete-mode t)
  (ac-config-default)
  
  ;; Face
  (set-face-background 'popup-scroll-bar-foreground-face "red3")

  ;; Trigger key
  (if (display-graphic-p)
      (global-set-key [(control ?/)] 'auto-complete)
    (ac-set-trigger-key "TAB"))

  (ac-linum-workaround)
  )

;;;;
;;;;             company
;;;;
(use-package company
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (add-to-list 'company-backends '(company-abbrev
                                   company-c-headers
                                   company-dabbrev
                                   company-dabbrev-code
                                   company-etags
                                   company-files
                                   company-keywords
                                   ;;company-tabnine
                                   ))
  (add-hook 'python-mode-hook
          (lambda ()
            (add-to-list (make-local-variable 'company-backends)
                         'company-anaconda)))
  
  (setq company-dabbrev-downcase nil)

  (use-package company-c-headers)
  (use-package company-tabnine)
  
  :bind(("C-." . company-complete)
        :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             )
  )


;;;;
;;;;            flyspell
;;;;
(use-package flyspell
  :if (not is-windows)
  :diminish flyspell-mode
  :config
  (dolist (hook '(text-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(change-log-mode-hook log-edit-mode-hook))
    (add-hook hook (lambda () (flyspell-mode -1))))

  (add-hook 'c++-mode-hook  (lambda () (flyspell-prog-mode)))
  :bind (:map flyspell-mode-map
              ("C-." . nil))
  )

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode)


;;;;
;;;;            macOS
;;;;
(when is-macos
  (use-package reveal-in-osx-finder
    :config (defalias 'open-in-finder 'reveal-in-osx-finder)
    )
  )

;;;;
;;;;            misc packages
;;;;
(use-package p4
  :diminish p4-mode
  :init (setenv "P4CONFIG" "p4.config")
  :bind (("C-4" . p4-edit)))


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

(fx/setup-c++)

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

(fx/setup-indent 4)


;;;;
;;;;         local packages
;;;;
(when t
  (setq local-packages-dir (concat root-dir "local-packages"))
  (when (file-directory-p local-packages-dir)
    (add-to-list 'load-path local-packages-dir))

  (use-package monkeyc-mode
    :ensure nil
    :mode "\\.mc\\'")
  
  (use-package asc-mode
    :ensure nil
    :mode "\\.asc\\'")
  )

;;;;
;;;;    Esko links
;;;;
(use-package browse-url
  :if (not is-windows))
(use-package goto-addr
  :if (not is-windows)
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


;;;;
;;;;     General keybindings
;;;;
(when t
  (global-set-key (kbd "M-m") 'set-mark-command)
  (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
  (global-set-key (kbd "M-b") (lambda () (interactive) (backward-word)))
  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "M-`") 'set-mark-command)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-x r") 'reload)
  ;;(global-set-key (kbd "C-S-n") 'ns-next-frame)
  (global-set-key (kbd "C-S-o") 'ns-next-frame)
  (global-set-key (kbd "M-`") 'ns-next-frame)
  (global-set-key (kbd "C-`") 'ns-next-frame)
  ;;(global-set-key (kbd "C-1") 'switch-buffer)
  (global-set-key (kbd "C-x j") 'kill-other-buffers)
  (global-set-key (kbd "C-t") 'new-scratch)

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
  ;; (global-set-key (kbd "C-/") 'comment-region)
  ;; (global-set-key (kbd "C-?") 'uncomment-region)

  ;; Mouse-3
  (global-set-key (kbd "<mouse-3>") 'mouse-popup-menubar-stuff)
  )

;;;;
;;;;          theme & customization
;;;;
(use-package solarized-theme)
(use-package cyberpunk-theme)
(use-package color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-blue t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
