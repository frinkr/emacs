(setq root-dir (file-name-directory (file-truename load-file-name)))
(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux
(setq is-snowmacs is-windows)   ;; slow platform

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


;;;;
;;;;           performance
;;;;
(setq gc-cons-threshold 1000000000)
(use-package benchmark-init
  :if (version< emacs-version "28.0")
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))


(defun buffer-file-path-or-directory()
  "Return current buffer file path or dired directory"
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name))
  )

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
;;;;           erase current buffer
;;;;
(defun fx-erase-buffer()
  (interactive)
  (when (y-or-n-p "Erase the buffer?")
    (erase-buffer)
    )
  )

;;;;
;;;;           copy file path clipboard
;;;;
(defun copy-file-path ()
  "Copy the current file path to the clipboard"
  (interactive)
  (let ((filename (buffer-file-path-or-directory)))
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
;;;;          open-with-default-application
;;;;
(defun open-with-default-application()
  "Copy current file with system default application"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (message filename)
      (cond (is-macos
             (shell-command (concat "open " (shell-quote-argument filename))))
            (is-windows
             (w32-shell-execute "open" filename))
            ))))


;;;;
;;;;        Core Settings
;;;;
(when t
  (global-hl-line-mode nil)

  ;; line & column number
  (line-number-mode t)
  (column-number-mode t)
  
  ;;(global-linum-mode (if is-snowmacs -1 t))
  (add-hook 'text-mode-hook 'display-line-numbers-mode)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  
  ;; mouse in terminal
  (xterm-mouse-mode t)

  ;; tool bar and menu bar
  (tool-bar-mode -1)
  (unless (display-graphic-p)
    (menu-bar-mode -1)
    )

  ;; Disable backup and autosave
  (setq backup-inhibited t)
  (setq auto-save-default nil)
  (save-place-mode t)   ;; save cursor position

  (blink-cursor-mode t)
  (setq-default cursor-type 'bar)
  (setq-default cursor-in-non-selected-windows 'hollow)
  
  (setq inhibit-splash-screen t)
  (setq transient-mark-mode t)
  (setq delete-key-deletes-forward t)
  (setq mouse-yank-at-point nil)
  (setq bookmark-save-flag 1)  ;; autosave bookmarks

  ;; indent
  (setq-default indent-tabs-mode nil)
  (when (fboundp 'electric-indent-local-mode) 
    (add-hook 'text-mode-hook (lambda () (electric-indent-local-mode -1))))

  (setq sp-escape-quotes-after-insert nil) ;; don't auto escape in smartparens-mode

  ;; upcase region is anoying
  (put 'upcase-region 'disabled nil)
  (put 'narrow-to-region 'disabled nil)

  ;; Setup time mode
  (setq display-time-day-and-date t)
  (setq display-time-format "%I:%M %Y/%m/%d")
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
  (when (not is-macos)
    (setq-default frame-title-format
                  (list '((buffer-file-name " %f"
                                            (dired-directory
                                             dired-directory
                                             (revert-buffer-function " %b"
                                                                     ("%b - Dir:  " default-directory))))))))

  ;; macOS transparent titlebar
  (when is-macos
    (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
    )

  ;; font
  (set-face-attribute 'default nil :family "Source Code Pro" :weight 'normal :height 140)

  ;; auto revert
  (global-auto-revert-mode)

  ;; Emacs27 has builtin
  (when (require 'display-fill-column-indicator nil 'noerror)
    (setq-default display-fill-column-indicator-character ?\u2506)
    (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
    (add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
    )
  
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
;;;;         dashboard
;;;;
(use-package dashboard
  :init
  (setq
   dashboard-set-init-info t
   dashboard-banner-logo-title "MAY THE FORCE BE WITH YOU!"
   dashboard-startup-banner (concat root-dir "banner.png")
   dashboard-items '((projects . 5)
                     (recents  . 15))
   dashboard-set-heading-icons t
   dashboard-set-file-icons t
   dashboard-set-footer nil
   )
  :config
  (dashboard-setup-startup-hook))

;;;;
;;;;           all-the-icons
;;;;
(use-package all-the-icons
  :config (setq all-the-icons-color-icons nil)
  )
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;;;;
;;;;           highlight-thing
;;;;
(use-package highlight
  :preface
  (defun fx/get-hlt-ov(begin end)
    (dolist (ov (overlays-in begin end))
      (when (overlay-get ov 'hlt-highlight) ;; check if been highlighed
        (return ov)
        )))
  
  (defun fx/smart-highlight()
    (interactive)
    (when-let* ((symbol (thing-at-point 'symbol))
                (bounds (bounds-of-thing-at-point 'symbol)))
      (if (fx/get-hlt-ov (car bounds) (cdr bounds))
          (hlt-unhighlight-symbol symbol)
        (hlt-highlight-symbol symbol)
        )))

  (defun fx/hlt-face-at-point()
    (when-let* ((symbol (thing-at-point 'symbol))
                (bounds (bounds-of-thing-at-point 'symbol))
                (ov (fx/get-hlt-ov (car bounds) (cdr bounds))))
      (if ov
          (overlay-get ov 'hlt-highlight)
        hlt-last-face
        )))
    
  (defun fx/cycle-highlight(next)
    (when (fx/get-hlt-ov 0 (buffer-size))
      (if next
          (hlt-next-highlight nil nil (fx/hlt-face-at-point) nil nil nil t)
        (hlt-previous-highlight nil nil (fx/hlt-face-at-point) nil nil t))
      ))

  (setq hlt-auto-faces-flag t)
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

  :bind(([double-mouse-1] . fx/smart-highlight)
        ("M-N" . (lambda() (interactive) (fx/cycle-highlight t)))
        ("M-P" . (lambda() (interactive) (fx/cycle-highlight nil))))
  )


;;;;
;;;;          Highlighting
;;;;
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  )
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :config
  (setq hl-paren-background-colors '("red"
                                     "DarkCyan"
                                     "DeepPink"
                                     "SpringGreen3"
                                     "MediumPurple1"
                                     "DarkOrange"
                                     "RoyalBlue1"
                                     ))
  (setq hl-paren-colors '("white"))
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
;;;;           save-as command
;;;;
(defun save-as(filename)
  (interactive "F")
  (write-region (point-min) (point-max) filename)
  )


;;;;
;;;;            which-key
;;;;
(use-package which-key
  :init (which-key-mode))

;;;;
;;;;        Dired setup
;;;;
(use-package dired-x
  :ensure nil
  :init
  (setq dired-details-hide-link-targets nil)
  :config
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ("^" . (lambda () (interactive) (find-alternate-file ".."))))
  )

(use-package dired-ranger
  :ensure t
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

;;;;
;;;;          Sidebar
;;;;
(use-package neotree
  :defer t
  :config (setq neo-theme 'icons))

;;;;
;;;;           bm
;;;;
(use-package bm
  :demand t
  :init
  (setq bm-restore-repository-on-load t)
  :config
  (setq bm-cycle-all-buffers t)
  (setq bm-repository-file "~/.emacs.d/bm-repository")
  (setq-default bm-buffer-persistence t)
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  :bind (("<left-margin> <mouse-1>" . bm-toggle-mouse))
  )

(use-package helm-bm
  :requires bm
  :init (defalias 'bm 'helm-bm)
  :bind(("C-c b" . helm-bm))
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
;;;;        vterm
;;;;
(use-package vterm
  :if (and module-file-suffix (not is-windows))
  :init (setq
         vterm-buffer-name-string "vterm %s"
         vterm-kill-buffer-on-exit t)
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
   helm-use-undecorated-frame-option t
   helm-echo-input-in-header-line nil ;; hide helm echo
   helm-buffers-fuzzy-matching t
   ;;helm-move-to-line-cycle-in-source t
   )

  (setq helm-ack-grep-executable "ack"
      helm-grep-default-command "ack -Hn --no-group --no-color %c %e %p %f"
      helm-grep-default-recurse-command "ack -H --no-group --no-color %c %e %p %f")

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

  (defun helm-display-buffer-popup-frame-fix-args (buffer frame-alist)
    (let* ((parent (selected-frame))
           (frame-geo (frame-geometry parent))
           (frame-pos (frame-position parent))
           (parent-left (car frame-pos))
           (parent-top (cdr frame-pos))
           (parent-width (* (frame-char-width parent) (frame-width parent)))
           (width (* (frame-char-width) (cdr (assoc 'width frame-alist))))
           (left (+ parent-left (/ (- parent-width width) 2)))
           (top (+ parent-top
                   (cdr (cdr (assoc 'title-bar-size frame-geo)))
                   (cdr (cdr (assoc 'menu-bar-size frame-geo)))
                   (cdr (cdr (assoc 'tool-bar-size frame-geo)))
                   )))
      (setcdr (assq 'left frame-alist) left)
      (setcdr (assq 'top frame-alist) top)
      )
    )
  
  :config
  (advice-add 'helm-score-candidate-for-pattern :around #'helm-score-candidate-fix)
  (advice-add 'helm-display-buffer-popup-frame :before #'helm-display-buffer-popup-frame-fix-args)
  (helm-autoresize-mode t)
  :bind
  (("M-l" . helm-semantic-or-imenu)
   ("C-S-l" . helm-semantic-or-imenu)
   ("C-x C-f" . helm-find-files)
   ("C-x C-r" . helm-recentf)
   ("C-x b" . helm-buffers-list)
   ("C-x C-b" . helm-mini)
   ("M-x" . helm-M-x)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-z"  . helm-select-action))
  )    ;; end of helm

(use-package helm-files
  :ensure nil
  :bind
  (:map helm-find-files-map
        ("C-s" . helm-ff-run-grep-ag)
        ("C-r" . helm-ff-run-query-replace-regexp)
        )
)

(use-package helm-ag
  :commands (helm-do-ag)
  :init (defalias 'hag 'helm-do-ag)
  :config
  (setq helm-ag-insert-at-point 'symbol)
  :bind
  ("C-S-s" . helm-do-ag))

(use-package ag
  :commands (ag ag-files ag-dired)
  :config
  (setq ag-highlight-search t))

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
  :commands (helm-projectile-on)
  :config
  (projectile-global-mode)
  (setq projectile-sort-order 'access-time)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'helm)
  (setq projectile-switch-project-action 'venv-projectile-auto-workon)
  (push 'helm-projectile-find-file helm-commands-using-frame)
  
  (defun my-find-file-at-point()
    "Find file at point based on context. See `helm-projectile-find-file-dwim'."
    (interactive)
      (let* ((project-root (projectile-project-root))
         (project-files (projectile-current-project-files))
         (files (projectile-select-files project-files)))
    (if (= (length files) 1)
        (find-file (expand-file-name (car files) (projectile-project-root)))
      (helm-projectile-find-other-file)
    )))
  
  (helm-projectile-on)
  :bind(("C-S-x C-S-f" . helm-projectile-find-file)
        ("C-S-o" . helm-projectile-find-file)
        ("C-1" . helm-projectile-find-other-file)
        ("C-S-b" . projectile-compile-project)
        :map c-mode-base-map
        ("C-2" . my-find-file-at-point))
  )


;;;;
;;;;       cmake
;;;;
;;(use-package rtags)
(use-package cmake-ide
  ;;:disabled ;; too slow
  ;;:requires rtags
  :init
  (setq cmake-ide-header-search-other-file nil
        cmake-ide-header-search-first-including nil)
  :config
  (cmake-ide-setup)
  )
(use-package cmake-mode
  :defer t)

;;;;
;;;;         dumb-jump
;;;;
(use-package dumb-jump
  :config (setq dumb-jump-selector 'helm)
  :ensure)

;;;;
;;;;       git
;;;;
(use-package magit
  :if (not is-snowmacs)
  :demand t)
(use-package diff-hl
  :if (not is-snowmacs)
  :demand t
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :bind (("C-S-p" . diff-hl-previous-hunk)
         ("C-S-n" . diff-hl-next-hunk))
  )
(use-package ediff
  :ensure nil
  :defer t
  :init
  (setq ediff-split-window-function 'split-window-horizontally
        ediff-window-setup-function 'ediff-setup-windows-plain)
  )
(use-package magit-filenotify
  :if (not is-snowmacs)
  :demand t
  :config (add-hook 'magit-status-mode-hook 'magit-filenotify-mode)
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
  :diminish company-mode
  :init
  (setq company-idle-delay 0
        company-minimum-prefix-length 1
        company-selection-wrap-around t)
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

  (setq company-dabbrev-ignore-case t
        company-dabbrev-downcase nil)

  (use-package company-c-headers)
  (use-package company-anaconda :defer t)
  ;;(use-package company-tabnine)
  
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
  :if (not is-snowmacs)
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
;;;;         reveal-in-folder   
;;;;
(use-package reveal-in-folder
  :init (defalias 'reveal-in-folder-this-buffer 'show-in-folder))

;;;;
;;;;         markdown
;;;;
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;;;
;;;;           uml 
;;;;
(use-package plantuml-mode
  :init (setq plantuml-default-exec-mode 'executable
              plantuml-indent-level 4)
  (with-eval-after-load "org"
    (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))
  (org-babel-do-load-languages 'org-babel-load-languages '((plantuml . t)))
  :mode (("\\.plantuml\\'" . plantuml-mode)
         ("\\.puml\\'" . plantuml-mode)
         )
  )

;;;;
;;;;           org
;;;;
(defun fx/org-cc (&optional ARG)
  (interactive)
  (org-ctrl-c-ctrl-c ARG)
  (org-display-inline-images)
  )
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-mode-hook
          (lambda ()
                  (local-set-key (kbd "C-c C-c") 'fx/org-cc)))
;;(define-key org-mode-map (kbd "C-c C-c") 'fx/org-cc)


;; ;;;;
;; ;;;;
;; ;;;;
;; (use-package lsp-mode :commands lsp :ensure t)
;; (use-package lsp-ui :commands lsp-ui-mode :ensure t)
;; (use-package company-lsp
;;   :ensure t
;;   :commands company-lsp
;;   :config (push 'company-lsp company-backends)) ;; add company-lsp as a backend
;; (use-package ccls
;;   :ensure t
;;   :config
;;   (setq ccls-executable "ccls")
;;   (setq lsp-prefer-flymake nil)
;;   (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-cppcheck c/c++-gcc))
;;   :hook ((c-mode c++-mode objc-mode) .
;;          (lambda () (require 'ccls) (lsp))))

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

(use-package dtrt-indent
  :init (defalias 'auto-indent-mode 'dtrt-indent-mode)
  )
(defun fx/setup-common-lang-indent(n)
  ;; java/c/c++
  (setq c-basic-offset n)
  )

(defun fx/setup-web-lang-indent(n)
  (setq coffee-tab-width n) ; coffeescript
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )

(fx/setup-common-lang-indent 4)
(fx/setup-web-lang-indent 2)


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

  (use-package help-fns+
    :if is-macos
    :ensure nil)

  (require 'clean-mode-line)
  (clean-mode-line-mode)

  (require 'esko-link-mode)
  (esko-link-mode)
  
  )

;;;;
;;;;     General keybindings
;;;;
(when t
  (setq fx-next-frame (if (fboundp 'ns-next-frame) 'ns-next-frame 'next-window-any-frame))
  
  (global-set-key (kbd "M-m") 'set-mark-command)
  (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
  (global-set-key (kbd "M-b") (lambda () (interactive) (backward-word)))
  (global-set-key (kbd "M-g") 'goto-line)
  (global-set-key (kbd "M-`") 'set-mark-command)
  (global-set-key (kbd "C-z") 'undo)
  (global-set-key (kbd "C-q") 'save-buffers-kill-terminal)
  (global-set-key (kbd "C-x r") 'reload)
  (global-set-key (kbd "C-S-k") 'fx-erase-buffer)
  ;;(global-set-key (kbd "C-S-n") 'ns-next-frame)
  ;;(global-set-key (kbd "C-S-o") fx-next-frame)
  (global-set-key (kbd "M-`") fx-next-frame)
  (global-set-key (kbd "C-`") fx-next-frame)
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

  ;; Mouse-3
  (global-set-key (kbd "<mouse-3>") 'mouse-popup-menubar-stuff)
  )

;;;;
;;;;          theme & customization
;;;;
(use-package solarized-theme :defer t)
(use-package cyberpunk-theme :defer t)
(use-package color-theme-sanityinc-tomorrow :defer t)
(use-package kaolin-themes :defer t) ;; kaolin-ocean !
(use-package remember-last-theme
  :ensure t
  :config (remember-last-theme-enable)
  )

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)
(put 'dired-find-alternate-file 'disabled nil)
