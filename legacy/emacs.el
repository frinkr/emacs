;;;;
;;;;                Global configs
;;;;
(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq is-wsl (eq system-type 'gnu/linux))  ;; windows subsystem for linux
(setq user-lisp-root (expand-file-name "~/.emacs.d/lisp"))

;;;;
;;;;           proxy
;;;;
(when nil
  (setq url-proxy-services
        '(("no_proxy" . "^\\(localhost\\|10.*\\)")
          ("http" . "eglbeprx001:8080")
          ("https" . "eglbeprx001:8080")))
  )

;;;;
;;;;                   melpa
;;;;
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  ;;(add-to-list 'package-archives '("milkbox" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize) ;; You might already have this line
  )

(defun fx/install-required-packages()  ;; install the required packages automatically
  (setq package-list '(
                       ac-octave
                       auto-complete
                       auto-virtualenv
                       back-button
                       beacon
                       bm
                       bison-mode
                       ;;cygwin-mount
                       cmake-font-lock
                       cmake-mode
                       cmake-ide
                       clang-format
                       counsel
                       dash
                       ;;dired+
                       ;;dired-details
                       ;;dired-details+
                       dired-hacks-utils
                       dired-k
                       dired-rainbow
                       dired-single
                       diminish
                       dockerfile-mode
                       ;;dos ;; use builtin bat-mode
                       dtrace-script-mode
                       ecb
                       elpy
                       engine-mode
                       fill-column-indicator
                       flycheck
                       flycheck-haskell
                       flycheck-ghcmod
                       flycheck-irony
                       ghc
                       ghci-completion
                       haskell-mode
                       helm
                       hlinum
                       highlight-parentheses
                       highlight-thing
                       highlight-symbol
                       highlight-numbers
                       ;;highlight-tail
                       lua-mode
                       ;;mouse3
                       magit
                       markdown-mode
                       nyan-mode
                       neotree
                       p4
                       phi-rectangle
                       py-autopep8
                       popup
                       pos-tip
                       ;;icicles
                       ido-vertical-mode
                       ivy
                       imenu-anywhere
                       on-screen
                       osx-dictionary
                       reveal-in-osx-finder
                       rtags
                       smooth-scroll
                       smooth-scrolling
                       tabbar
                       tabbar-ruler
                       xcscope
                       yascroll
                       yaml-mode
                       undo-tree

                       ;; top themes
                       zenburn-theme
                       solarized-theme
                       alect-themes
                       monokai-theme
                       moe-theme
                       material-theme
                       dracula-theme
                       afternoon-theme
                       )
        )


  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))

  )

(fx/install-required-packages)

;; some package (gud.el) managed manually
(add-to-list 'load-path user-lisp-root)



;;;;---------------------------------------------------------------------------
;;;;                     General Settings
;;;;---------------------------------------------------------------------------


;;;;
;;;;        Behavior Settings
;;;;
(defun fx/general-behavior()
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

  ;; recent files
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-menu-items 2500)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)

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

(fx/general-behavior)

;;;;
;;;;           a few visual tweaks
;;;;
(when t
  ;; Auto close bracket insertion
  (electric-pair-mode 1)

  ;; Show matching pairs of parenthess
  (when nil
    (show-paren-mode nil)
    (setq show-paren-when-point-in-periphery t)
    (setq show-paren-when-point-inside-paren t)
    (setq show-paren-style 'parenthesis)
    )

  (beacon-mode t)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-when-window-changes t)
  (setq beacon-blink-when-window-scrolls t)
  (setq beacon-blink-when-point-moves-horizontally t)
  (setq beacon-blink-when-point-moves-vertically t)

  (require 'highlight-parentheses)
  (global-highlight-parentheses-mode t)

  (set-face-attribute
   'hl-paren-face nil
   :slant 'italic
   )

  (require 'highlight-numbers)
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  )

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
;;;;           Set the emacs window size
;;;;
(defun fx/set-frame-size-according-to-resolution ()
  (interactive)
  (if is-windows
      (progn
        ;; use 120 char wide window for largeish displays
        ;; and smaller 80 column windows for smaller displays
        ;; pick whatever numbers make sense for you
        (if (> (x-display-pixel-width) 1280)
            (add-to-list 'default-frame-alist (cons 'width 120))
          (add-to-list 'default-frame-alist (cons 'width 80)))
        ;; for the height, subtract a couple hundred pixels
        ;; from the screen height (for panels, menubars and
        ;; whatnot), then divide by the height of a char to
        ;; get the height we want
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 200)
                                      (frame-char-height)))))))

(when (display-graphic-p)
  (fx/set-frame-size-according-to-resolution))


;;;;
;;;;      Global key bindings
;;;;
(defun fx/setup-key-bindings()
  ;; Set control & meta key
  (setq mac-option-key-is-meta nil)
  (setq mac-command-key-is-meta nil)
  (setq mac-command-modifier 'control)
  (setq mac-option-modifier 'meta)

  ;; set of glocal shortcut
  (define-key global-map "\C-xw" 'what-line)
  (define-key global-map "\C-z" 'undo)
  (define-key global-map "\M-g" 'goto-line)
  (define-key global-map "\C-o" 'ff-find-other-file)
  (define-key global-map "\C-q" 'save-buffers-kill-terminal)
  (define-key global-map [(meta m)] 'set-mark-command)
  (define-key global-map [delete] 'delete-char)
  (define-key global-map [backspace] 'delete-backward-char)
  (define-key isearch-mode-map [backspace] 'isearch-delete-char)

  (global-set-key (kbd "M-SPC") 'set-mark-command)
  (global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
  (global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
  (global-set-key (kbd "C-4") 'p4-edit)

  (when is-windows
    (define-key global-map [f1] 'help-command)
    (define-key global-map [f2] 'undo)
    (define-key global-map [f3] 'isearch-forward)
    (define-key global-map [f4] 'other-window)
    (define-key global-map [f12] 'revert-buffer)
    (define-key global-map [button4] 'previous-line)
    (define-key global-map [button5] 'next-line)
    )
  )

(fx/setup-key-bindings)


;;;;
;;;;          new scratch
;;;;

(when t
  (defun new-scratch ()
    "open up a guaranteed new scratch buffer"
    (interactive)
    (switch-to-buffer (loop for num from 0
                            for name = (format "*scratch-%03i*" num)
                            while (get-buffer name)
                            finally return name)))

  (global-set-key (kbd "C-t") 'new-scratch)
  (global-set-key (kbd "<header-line> <double-mouse-1>") 'new-scratch)
  )


;;;;
;;;;          TODO
;;;;
(when t
  (defun default-todo-list()
    "open the default todo list"
    (interactive)
    (find-file "~/.emacs.d/todo.org")
    )
  (global-set-key (kbd "C-S-t") 'default-todo-list)

  (add-hook 'org-agenda-mode-hook
            (lambda ()
              (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
              (auto-save-mode)))
  )


;;;;
;;;;          undo-tree-mode
;;;;
(defun fx/setup-undo()
  (require 'undo-tree)
  ;; override the function so undo-tree-mode can be
  ;; force enabled
  (defun undo-tree-overridden-undo-bindings-p ())
  (global-undo-tree-mode)

  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  )
(fx/setup-undo)

;;;;
;;;;          ido
;;;;
(defun fx/setup-ido()
  (require 'ido)
  (require 'ido-vertical-mode)

  (setq ido-everywhere t)

  (ido-mode t)
  (ido-vertical-mode t)
  )
(fx/setup-ido)

;;;;
;;;;      kill minibuffer
;;;;
(defun fx/setup-minibuffer()
  (defun fx/kill-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))

  (add-hook 'mouse-leave-buffer-hook 'fx/kill-minibuffer)
  )
(fx/setup-minibuffer)

;;;;
;;;;          ibuffer
;;;;
(defun fx/setup-ibuffer()
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t)
  )
(fx/setup-ibuffer)


;;;;
;;;;          scrolling
;;;;
(defun fx/setup-scrolling()
  (require 'on-screen)
  (on-screen-global-mode nil)

  ;;  (require 'smooth-scroll)
  ;;  (smooth-scroll-mode t)

  (require 'smooth-scrolling)
  (when t
    (setq mouse-wheel-progressive-speed nil) ;; make the scrolling slower
    (setq mouse-wheel-scroll-amount '(5 ((shift) . 5))) ;; 3 lines at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 5) ;; keyboard scroll 3 lines at a time
    )


  ;; yascroll
  (when t
    (require 'yascroll)
    (global-yascroll-bar-mode 1)
    (setq yascroll:delay-to-hide nil)

    (scroll-bar-mode -1)
    )
  )
;;;; (fx/setup-scrolling)

;;;;
;;;;           bookmark
;;;;
(defun fx/setup-bookmark()
  (require 'bm)
  (defun fx/bm-get-line-at-click ()
    (save-excursion
      (let ((click-y (cdr (cdr (mouse-position))))
            (line-move-visual-store line-move-visual))
        (setq line-move-visual t)
        (goto-char (window-start))
        (next-line (1- click-y))
        (setq line-move-visual line-move-visual-store)
        (line-number-at-pos))))


  (defun fx/bm-line-at-click ()
    (interactive)
    (goto-line (fx/bm-get-line-at-click))
    (bm-toggle)
    )

  (global-set-key (kbd "<left-margin> <mouse-1>") 'fx/bm-line-at-click)

  (setq bm-highlight-style 'bm-highlight-line-and-fringe)

  ;; save and restore
  (when t
    (setq bm-repository-file "~/.emacs.d/bm-repository")
    (setq-default bm-buffer-persistence t)
    (add-hook' after-init-hook 'bm-repository-load)
    (add-hook 'find-file-hooks 'bm-buffer-restore)
    (add-hook 'kill-buffer-hook #'bm-buffer-save)

    (add-hook 'kill-emacs-hook #'(lambda nil
                                   (bm-buffer-save-all)
                                   (bm-repository-save)))

    (add-hook 'after-save-hook #'bm-buffer-save)

    (add-hook 'find-file-hooks   #'bm-buffer-restore)
    (add-hook 'after-revert-hook #'bm-buffer-restore)
    )
  )
(fx/setup-bookmark)

;;;;
;;;;           nyan
;;;;
(when t
  (require 'nyan-mode)
  (nyan-mode t)
  )


;;;;
;;;;           formatter
;;;;
(when t
  (require 'clang-format)
  (defun untabify-buffer ()
    "indent & untabify the buffer"
    (interactive)
    (save-excursion
      (delete-trailing-whitespace)
      (indent-region (point-min) (point-max) nil)
      (untabify (point-min) (point-max))
      )
    ))


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

(global-set-key (kbd "C-x j") 'kill-other-buffers)


;;;;
;;;;           eshell
;;;;

(defun fx/setup-eshell()

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

(fx/setup-eshell)

;;;;
;;;;           cygwin in emacs
;;;;
(defun fx/setup-cygwin()
  ;; Cgywin Emacs
  (when (eq system-type 'cygwin)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)
    )

  (setq cygwin-root "C:/cygwin/")

  ;; NTEmacs
  (when (and is-windows (file-exists-p cygwin-root))
    (setq cygwin-mount-cygwin-bin-directory (concat cygwin-root "bin"))
    (setenv "PATH" (concat (concat cygwin-root "bin;") (getenv "PATH")))
    (setq exec-path (cons (concat cygwin-root "bin;") exec-path))

    (require 'cygwin-mount)
    (cygwin-mount-activate)

    ;; Remove the unsightly chars
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)

    (when nil
      (setq explicit-shell-file-name (concat cygwin-root "Cygwin_x86_64 vc12.bat"))
      ;; For subprocesses invoked via the shell
      ;; (e.g., "shell -c command")
      (setq shell-file-name explicit-shell-file-name)
      )

    (when t
      ;; NT-emacs assumes a Windows shell. Change to bash.
      (setq shell-file-name "bash")
      (setenv "SHELL" shell-file-name)
      (setq explicit-shell-file-name shell-file-name)
      )

    (when t
      ;; Prevent issues with the Windows null device (NUL)
      ;; when using cygwin find with rgrep.
      (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device)
        "Use cygwin's /dev/null as the null-device."
        (let ((null-device "/dev/null"))
          ad-do-it))
      (ad-activate 'grep-compute-defaults)
      )
    ;;   add more path for emacs (eshell)
    ;;
    ;;(when (string-equal system-type "windows-nt")
    (if nil
        (setq exec-path
              (append exec-path
                      '(
                        "C:/Windows/system32/"
                        "C:/Windows/"
                        )
                      )))
    ;;           emacs prompt for eshell
    ;;
    (setq eshell-prompt-function
          (lambda ()
            (concat (getenv "USERNAME") "@"
                    (file-name-nondirectory (eshell/pwd))
                    (if (= (user-uid) 0) " # " " $ "))))

    ;; shell-prompt not work for Windows.
    ;; We need fix .bashrc for windows (NOT .bash_profile, which is not read
    ;; by cygwin version of emacs)
    ;; export PS1="daji% "
    ;;
    ;; STARTFGCOLOR='\e[0;35m';
    ;; STARTBGCOLOR="\e[40m"
    ;; ENDCOLOR="\e[0m"
    ;;
    ;; if [ "$PS" == "" ] ; then
    ;;     export PS1="daji\$ "
    ;;     if [ "$TERM" == "xterm" ] ; then
    ;;         export PS1="\[$STARTFGCOLOR$STARTBGCOLOR\]daji:\W\$ \[$ENDCOLOR\]"
    ;;     elif [ "$TERM" == "cygwin" ] ; then
    ;;         export PS1="\[$STARTFGCOLOR$STARTBGCOLOR\]daji:\W\$ \[$ENDCOLOR\]"
    ;;     fi
    ;; fi
    ;;
    ;; if [ "$TERM" == "xterm" ] ; then
    ;;     PROMPT_COMMAND='echo -ne "\e]0;${HOSTNAME} - ${PWD}\007"'
    ;; fi

    ))

(fx/setup-cygwin)

;;;;
;;;;          diff
;;;;
(when t
  (setq-default ediff-forward-word-function 'forward-char)
  )


;;;;
;;;;           google
;;;;
(when t
  (require 'engine-mode)
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
;;;;       macOS setup
;;;;
(defun fx/setup-macos()
  ;; drag on Mac OSX
  (global-set-key [ns-drag-file] 'ns-find-file)
  (setq ns-pop-up-frames nil)

  ;; reveal in Finder
  (require 'reveal-in-osx-finder)
  (defalias 'open-in-finder 'reveal-in-osx-finder)

  )
(fx/setup-macos)


;;;;
;;;;           kill *Completions* buffer automatically
;;;;
(defun fx/kill-completion-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*")))
  output)

(add-hook 'comint-preoutput-filter-functions 'fx/kill-completion-buffer)


;;;;
;;;;           reload the buffer
;;;;
(defun reload ()
  "Reload the buffer w/o prompt."
  (interactive)
  (revert-buffer nil t))

(global-set-key (kbd "C-x r") 'reload)


;;;;
;;;;           set buffer modified
;;;;
(defun touch()
  "Touch the buffer"
  (interactive)
  (set-buffer-modified-p t))


;;;;
;;;;           open file with sudo
;;;;
(defun find-file-sudo ()
  "Open current buffer as root!"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo::"
             buffer-file-name))))

(global-set-key (kbd "C-x C-k") 'find-file-sudo)


;;;;
;;;;           set emacs PATH
;;;;
(defun fx/set-exec-path ()
  (let ((path-from-shell
         (replace-regexp-in-string "[[:space:]\n]*$" ""
                                   (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when is-macos
  (fx/set-exec-path))


;;;;
;;;;           mouse3
;;;;
(when nil
  (when (display-graphic-p)
    (require 'mouse3)))

;;;;
;;;;           neotree
;;;;
(when t
  (require 'neotree)
  )


;;;;
;;;;           icicles
;;;;
(when nil
  (require 'icicles)
  (icy-mode t)
  (setq icicle-touche-pas-aux-menus-flag t)
  )


;;;;
;;;;           highlight-thing
;;;;
(defun fx/setup-highlight-thing()
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

  ;;  (require 'highlight-thing)
  ;;  (global-highlight-thing-mode)
  ;;  (setq highlight-thing-delay-seconds 0.2)
  )
(fx/setup-highlight-thing)

;;;;
;;;;           fill column indicator
;;;;
(defun fx/setup-column-indicator()
  (require 'fill-column-indicator)
  (setq fci-rule-width 1)
  (setq fci-rule-column 90)
  (when nil
    (if (display-graphic-p)
        (setq fci-rule-color "darkblue")
      (setq fci-rule-color "green"))
    )

  (add-hook 'after-change-major-mode-hook 'fci-mode)

  ;; will disrupt the auto-complte menu, hide the indicator
  ;; when popup is on
  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
    (when fci-mode
      (turn-off-fci-mode)))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode)))
  )
(fx/setup-column-indicator)

;;;;
;;;;           line number
;;;;
(defun fx/setup-line-number()
  (require 'linum)
  (add-hook 'find-file-hook (lambda () (linum-mode 1)))

  ;; better appearance for terminal
  (when (not (display-graphic-p))
    (defun fx/linum-format-func (line)
      (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
        (propertize (format (format "%%%dd| " w) line) 'face 'linum)))
    (setq linum-format 'fx/linum-format-func)
    )

  (global-linum-mode 1)
  (linum-mode t)
  )
(fx/setup-line-number)

;;;;
;;;;           P4
;;;;
(when t
  (load-library "p4")
  (setenv "P4CONFIG" "p4.config"))


;;;;
;;;;           magit
;;;;

(defun fx/setup-git()
  (require 'magit)

  (setq magit-auto-revert-mode nil)

  (global-set-key (kbd "C-x g") 'magit-status)
  (defalias 'magit 'magit-status)
  )
(fx/setup-git)

;;;;
;;;;           tabbar
;;;;
(defun fx/setup-tabbar()
  (setq tabbar-ruler-global-tabbar t)
  (setq tabbar-ruler-use-mode-icons nil)  ;; don't use the ungly icons
  (setq tabbar-ruler-swap-faces t)
  (require 'tabbar-ruler)

  ;; use my faces
  (defun tabbar-diff-face-p (face)
    (facep face)
    )

  (defun tabbar-install-faces (&optional frame)
    ;; do nothing
    )

  (require 'tabbar)
  (tabbar-mode 1)

  (when (not (display-graphic-p))
    (global-set-key (kbd "M-,") 'tabbar-backward)
    (global-set-key (kbd "M-.") 'tabbar-forward))
  (global-set-key (kbd "C-{") 'tabbar-backward)
  (global-set-key (kbd "C-}") 'tabbar-forward)

  (setq tabbar-buffer-groups-function
        (lambda ()
          (list "All Buffers")))

  ;; Remove buffer names from list
  (setq *tabbar-ignore-buffers* '("*CEDET Global*" "*Compile-Log*" "*Messages*" "*etags tmp*" ))
  (setq tabbar-buffer-list-function
        (lambda ()
          (cl-remove-if
           (lambda (buffer)
             (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
                  (cl-loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
                           thereis (string-equal (buffer-name buffer) name))))
           (tabbar-buffer-list))))

  ;; Add a buffer modification state indicator in the tab label, and place a
  ;; space around the label to make it looks less crowd.
  (defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
    (setq ad-return-value
          (if (and (buffer-modified-p (tabbar-tab-value tab))
                   (buffer-file-name (tabbar-tab-value tab)))
              (concat " * " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))
  ;; Called each time the modification state of the buffer changed.
  (defun fx/modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  ;; First-change-hook is called BEFORE the change is made.
  (defun fx/on-buffer-modification ()
    (set-buffer-modified-p t)
    (fx/modification-state-change))
  (add-hook 'after-save-hook 'fx/modification-state-change)
  ;; This doesn't work for revert, I don't know.
  ;;(add-hook 'after-revert-hook 'fx/modification-state-change)
  (add-hook 'first-change-hook 'fx/on-buffer-modification)

  (setq tabbar-use-images nil)
  (setq tabbar-separator (quote (0.2)))
  )

(fx/setup-tabbar)

;;;;
;;;;           Dired
;;;;
(defun fx/setup-dired()
  ;; show details by default
  ;;(setq diredp-hide-details-initially-flag nil)
  ;;(require 'dired+)
  ;;(require 'dired-details+)

  ;; show link
  (setq dired-details-hide-link-targets nil)

  ;; hide uninteresting files
  (setq-default dired-omit-mode t)
  ;;(setq-default dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.")
  (setq-default dired-omit-files "^\\.Spotlight-V100$\\|^\\.apdisk$\\|^\\.TemporaryItems$\\|^\\.Trashes$\\|^\\.fseventsd$\\|^\\.DocumentRevisions-V100$\\|^\\.#a\\|^\\.DS_Store$")
  (add-to-list 'dired-omit-extensions ".Spotlight-V100")
  (add-to-list 'dired-omit-extensions ".TemporaryItems")
  (add-to-list 'dired-omit-extensions ".Trashes")
  (add-to-list 'dired-omit-extensions ".apdisk")
  (add-to-list 'dired-omit-extensions ".com.apple.timemachine.supported")
  (add-to-list 'dired-omit-extensions ".fseventsd")

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

  (add-hook 'dired-mode-hook 'fx/dired-mode-hook)
  (defun fx/dired-mode-hook ()
    (local-set-key (kbd "<mouse-2>") 'diredp-mouse-find-file-reuse-dir-buffer))

  )
;;(fx/setup-dired)

;;;;
;;;;            Mouse Scroll in terminal
;;;;
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


;;;;
;;;;           fast-nav mode
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
      (global-set-key (kbd "<mouse-2>") (lambda () (interactive) (kill-buffer (current-buffer))))
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


;;;;
;;;;           mode line
;;;;
(when t
  (require 'diminish)

  (eval-after-load "flycheck" '(diminish 'flycheck-mode nil))
  (eval-after-load "auto-complete" '(diminish 'auto-complete-mode nil))
  (eval-after-load "p4" '(diminish 'p4-mode nil))
  (eval-after-load "abbrev" '(diminish 'abbrev-mode nil))

  (diminish 'fx/fast-nav-mode nil)
  )



;;;;
;;;;           rectangular select
;;;;
(when t
  (require 'phi-rectangle)
  )

;;;;
;;;;           back-bottom
;;;;
(when t
  (require 'back-button)
  (back-button-mode 1)
  )

;;;;
;;;;           ansi color
;;;;
(when t
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )


;;;;
;;;;                     theme
;;;;
(defun fx/setup-theme()
  (when t ;; basic
    (if (display-graphic-p)
        (progn
          (setq default-cursor-type '(bar . 2))
          (setq cursor-in-non-selected-windows 'hollow)
          ;;(set-face-background hl-line-face "gray13")
          ;;(set-cursor-color "red")
          ;;(set-face-foreground 'minibuffer-prompt "yellow")
          ;;(set-face-foreground 'custom-face-tag "cyan")
          ;;(set-face-background 'region "SeaGreen4")
          )
      (progn
        (menu-bar-mode -1)
        ;;        (set-face-background 'hl-line "color-235")
        ;;        (set-face-attribute 'region nil :background "color-19")
        )
      )
    )

  (when t

    (defun what-face (pos)
      (interactive "d")
      (let ((face (or (get-char-property (point) 'read-face-name)
                      (get-char-property (point) 'face))))
        (if face (message "Face: %s" face) (message "No face at %d" pos))))

    ;; Font stuff for emacs versions that support it
    ;;  (this stuff seems to be the least portable, comment this stuff out if it
    ;;   prevents the config file from loading)
    (when is-macos
      ;;(set-face-attribute 'default nil :font "Menlo 12")
      ;;(set-face-attribute 'default nil :font "Anonymous Pro-14")
      (set-face-attribute 'default nil :family "Source Code Pro" :weight 'light :height 130)

      ;; fix gap at top when maximized
      (setq frame-resize-pixelwise t)
      )

    (when is-windows
      (set-face-attribute 'default nil :font "Consolas 11"))

    (when is-wsl
      (set-face-attribute 'default nil :font "DejaVu Sans Mono-14"))
    )


  (defun fx/setup-tabbar-theme()
    "setup the the tabbar-theme, which is not included in most theme"

    (set-face-attribute
     'tabbar-default nil
     :inherit 'default
     :foreground (face-attribute 'mode-line :foreground)
     :background (face-attribute 'mode-line :background)
     :family (face-attribute 'mode-line :family)
     :height (face-attribute 'mode-line :height)
     :weight (face-attribute 'mode-line :weight)
     :underline nil
     :box nil
     )

    (set-face-attribute
     'tabbar-unselected nil
     :inherit 'tabbar-default
     :foreground (face-attribute 'tabbar-default :foreground)
     :background (face-attribute 'tabbar-default :background)
     :box '(:style released-button)
     )

    (set-face-attribute
     'tabbar-modified nil
     :inherit 'tabbar-default
     :weight 'bold
     :slant 'italic
     :background (face-attribute 'tabbar-default :background)
     :box '(:style released-button)
     )

    (set-face-attribute
     'tabbar-unselected-modified nil
     :inherit 'tabbar-default
     :weight 'normal
     :slant 'italic
     :background (face-attribute 'tabbar-default :background)
     :foreground (face-attribute 'tabbar-default :foreground)
     :box '(:style released-button)
     )


    (set-face-attribute
     'tabbar-selected nil
     :inherit 'tabbar-default
     :foreground (face-attribute 'font-lock-function-name-face :foreground)
     :background (face-attribute 'tabbar-default :background)
     :weight 'bold
     :height (face-attribute 'tabbar-default :height)
     :slant 'normal
     :box '(:style pressed-button)
     )

    (set-face-attribute
     'tabbar-selected-modified nil
     :inherit 'tabbar-default
     :slant 'italic
     :weight 'bold
     :height (face-attribute 'tabbar-default :height)
     :box '(:style pressed-button))

    (set-face-attribute
     'tabbar-highlight nil
     :inherit 'highlight
     :underline nil)


    (set-face-attribute
     'tabbar-button nil
     :inherit 'tabbar-default
     :foreground (face-attribute 'tabbar-default :foreground)
     :background (face-attribute 'tabbar-default :background)
     :box '(:line-width 1 :style released-button)
     )

    (set-face-attribute
     'tabbar-separator nil
     :inherit 'tabbar-default
     :foreground (face-attribute 'tabbar-default :foreground)
     :background (face-attribute 'tabbar-default :background)
     :box nil
     )

    (tabbar-install-faces)

    (set-face-attribute
     'tabbar-selected nil
     :weight 'bold
     :box '(:style pressed-button)
     )

    )

  (defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")
  (defadvice load-theme (after run-after-load-theme-hook activate)
    "Run `after-load-theme-hook'."
    (run-hooks 'after-load-theme-hook))

  (add-hook 'after-load-theme-hook 'fx/setup-tabbar-theme)

  (load-theme 'dracula t)
  ;;(load-theme 'solarized-light t)
  ;;(load-theme 'monokai t)
  ;;(load-theme 'gotham t)
  ;;(load-theme 'afternoon t)

  )

(fx/setup-theme)



;;;;---------------------------------------------------------------------------
;;;;                     Modes Settings
;;;;---------------------------------------------------------------------------

(defun fx/setup-modes()
;;;;
;;;;           lua
;;;;
(when t
  (autoload 'lua-mode "lua-mode" "Lua editing mode." t)
  (add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode))
  )

;;;;
;;;;           yaml mode
;;;;
(when t
  (require 'yaml-mode)
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  )


;;;;
;;;;           octave
;;;;
(when t
  (setq auto-mode-alist
        (cons '("\\.m$" . octave-mode) auto-mode-alist))

  (setq inferior-octave-prompt ">> ")

  (add-hook 'octave-mode-hook
            (lambda ()
              (abbrev-mode 1)
              (auto-fill-mode 1)
              (font-lock-mode 1)))

  (require 'ac-octave)
  ;;  (ac-octave-init)
  (defun fx/ac-octave-mode-setup ()
    (setq ac-sources '(ac-source-octave)))
  (add-hook 'octave-mode-hook
            '(lambda () (fx/ac-octave-mode-setup)))

  (add-hook 'inferior-octave-mode-hook
            (lambda ()
              (turn-on-font-lock)
              (define-key inferior-octave-mode-map [up]
                'comint-previous-input)
              (define-key inferior-octave-mode-map [down]
                'comint-next-input)))
  )


;;;;
;;;;           Haskell mode
;;;;
(when t
  (setq exec-path (append exec-path '("/Users/frinkr/.cabal/bin")))

  ;; `cabal install ghc-mod` first
  (require 'haskell-mode)
  (add-to-list 'Info-default-directory-list (concat user-lisp-root "haskell-mode"))
  (autoload 'ghc-init "ghc" nil t)
  (autoload 'ghc-debug "ghc" nil t)
  (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-font-lock)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

  (eval-after-load 'haskell-mode '(progn
                                    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
                                    (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
                                    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
                                    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
                                    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
  (eval-after-load 'haskell-cabal '(progn
                                     (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
                                     (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
                                     (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
                                     (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

  (custom-set-variables
   '(haskell-interactive-mode-hide-multi-line-errors nil)
   '(haskell-process-log t)
   '(haskell-process-type (quote cabal-repl)))
  )


;;;;
;;;;           cmake
;;;;
;; Add cmake listfile names to the mode list.
(when t
  (setq auto-mode-alist
        (append
         '(("CMakeLists\\.txt\\'" . cmake-mode))
         '(("\\.cmake\\'" . cmake-mode))
         '(("CMakeCache\\.txt\\'" . cmake-mode))
         auto-mode-alist))

  (autoload 'cmake-mode "cmake-mode" nil t)
  (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
  (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
  )

;;;;
;;;;           Makefile
;;;;
(when t
  (add-to-list 'auto-mode-alist '("\\.mk\\'" . makefile-mode))
  )

;;;;
;;;;           yacc and lex
;;;;
(when t
  (require 'bison-mode)
  (add-to-list 'auto-mode-alist '("\\.lm\\'" . bison-mode))
  (add-to-list 'auto-mode-alist '("\\.ym\\'" . bison-mode))
  )

;;;;
;;;;           D mode
;;;;
(when t
  (autoload 'd-mode "d-mode" "Major mode for editing D code." t)
  (add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode)))


;;;;
;;;;           DTrace
;;;;
(when t
  (autoload 'dtrace-script-mode "dtrace-script-mode" () t)
  (add-to-list 'auto-mode-alist '("\\.d\\'" . dtrace-script-mode))
  (add-hook 'dtrace-script-mode-hook 'imenu-add-menubar-index)
  (add-hook 'dtrace-script-mode-hook 'font-lock-mode))


;;;;
;;;;                        pseudocode mode
;;;;
(when t
  (require 'generic-x)

  (defface pseudocode-paragraph-face
    '((t (:foreground "green"
                      :background "gray20"
                      :slant italic
                      :height 1.1
                      ))
      )
    "pseudocode paragraph face")

  (defvar pseudocode-mode-font-lock-keywords
    '(("\\<m\\>\\|\\<n\\>\\|\\<p\\>\\|\\<k\\>\\|\\<i\\>\\|\\<j\\>\\|\\<nil\\>" . font-lock-variable-name-face)
      ("\\<node\\>\\|\\<value\\>\\|\\<prev\\>\\|\\<next\\>\\|\\<head\\>" . font-lock-variable-name-face)
      ("\\<from\\>\\|\\<to\\>" . font-lock-keyword-face)
      ("\\<done\\>\\|\\<end\\>\\|\\<fi\\>" . font-lock-keyword-face)
      ("\\<is\\>\\|\\<fi\\>" . font-lock-keyword-face)
      ("^[0-9]+\..*" . 'pseudocode-paragraph-face)
      ("\:" . font-lock-keyword-face)
      )
    )

  (define-derived-mode pseudocode-mode c++-mode
    (font-lock-add-keywords nil pseudocode-mode-font-lock-keywords)
    (setq c-syntactic-indentation nil)
    (setq mode-name "pseudocode mode")
    )

  (add-to-list 'auto-mode-alist '("\\.answer$" . pseudocode-mode))
  )


;;;;
;;;;        Markdown mode
;;;;
(when t
  (require 'markdown-mode)
  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

  (autoload 'gfm-mode "markdown-mode"
    "Major mode for editing GitHub Flavored Markdown files" t)
  (add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
  )


;;;;
;;;;           DOS mode for batch
;;;;
(when t
  (require 'bat-mode)
  (add-to-list 'auto-mode-alist '("\\.bat$" . bat-mode)))


;;;;
;;;;           Dockerfile mode
;;;;
(when t
  (require 'dockerfile-mode)
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))


)

(fx/setup-modes)

;;;;---------------------------------------------------------------------------
;;;;                     Programming Settings
;;;;---------------------------------------------------------------------------

;;;;
;;;;           C++11
;;;;
(defun fx/setup-c++11() ;; depricated in emacs 25
  (require 'font-lock)

  (defun --copy-face (new-face face)
    "Define NEW-FACE from existing FACE."
    (copy-face face new-face)
    (eval `(defvar ,new-face nil))
    (set new-face new-face))

  (--copy-face 'font-lock-label-face  ; labels, case, public, private, proteced, namespace-tags
               'font-lock-keyword-face)
  (--copy-face 'font-lock-doc-markup-face ; comment markups such as Javadoc-tags
               'font-lock-doc-face)
  (--copy-face 'font-lock-doc-string-face ; comment markups
               'font-lock-comment-face)

  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t)


  (add-hook 'c++-mode-hook
            '(lambda()
               (font-lock-add-keywords
                nil '(;; complete some fundamental keywords
                      ("\\<\\(void\\|unsigned\\|signed\\|char\\|short\\|bool\\|int\\|long\\|float\\|double\\)\\>" . font-lock-keyword-face)
                      ;; add the new C++11 keywords
                      ("\\<\\(alignof\\|alignas\\|constexpr\\|decltype\\|noexcept\\|nullptr\\|static_assert\\|thread_local\\|override\\|final\\)\\>" . font-lock-keyword-face)
                      ("\\<\\(char[0-9]+_t\\)\\>" . font-lock-keyword-face)
                      ;; PREPROCESSOR_CONSTANT
                      ("\\<[A-Z]+[A-Z_]+\\>" . font-lock-constant-face)
                      ;; hexadecimal numbers
                      ("\\<0[xX][0-9A-Fa-f]+\\>" . font-lock-constant-face)
                      ;; integer/float/scientific numbers
                      ("\\<[\\-+]*[0-9]*\\.?[0-9]+\\([ulUL]+\\|[eE][\\-+]?[0-9]+\\)?\\>" . font-lock-constant-face)
                      ;; user-types (customize!)
                      ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\|type\\|ptr\\)\\>" . font-lock-type-face)
                      ("\\<\\(xstring\\|xchar\\)\\>" . font-lock-type-face)
                      ))
               ) t)
  )


;;;;
;;;;           Auto Complete
;;;;
(defun fx/setup-auto-complete()
  (require 'auto-complete-config)
  (require 'pos-tip)  ;; for a nice completion menu and help

  (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

  (ac-config-default)
  (setq ac-auto-start t)

  ;; tooltip
  (setq ac-use-quick-help t)

  ;; menu font
  ;;(set-face-font 'ac-candidate-face "Courier New 13")
  ;;(set-face-font 'ac-selection-face "Courier New 13")

  ;; scroll bar font
  (set-face-background 'popup-scroll-bar-foreground-face "red3")
  (if (display-graphic-p)
      (global-set-key [(control ?/)] 'auto-complete)
    (ac-set-trigger-key "TAB"))

  ;; matching
  (setq ac-use-fuzzy t)
  (setq ac-ignore-case t)

  ;;(setq popup-use-optimized-column-computation nil)
  (ac-linum-workaround)
  )
(fx/setup-auto-complete)

;;;;
;;;;           ecb
;;;;
(defun fx/setup-ecb()
     (when is-macos
       ;; error when byte-compiling from melpa, nerver mind. It can just run

       (require 'ecb)
       (custom-set-variables '(ecb-options-version "2.40"))

       ;; activate at start up
       (when (display-graphic-p)
         (setq ecb-auto-activate t))

       (setq ecb-auto-activate nil)

       ;; no tip-of-day
       (setq ecb-tip-of-the-day nil)

       ;; layout
       (setq ecb-layout-name "left15")
       (setq ecb-layout-window-sizes nil)
       (setq ecb-fix-window-size (quote width)) ;; fixed witdh

       ;; directories
       (setq ecb-source-path (quote (("/usr/include" "c")
                                     ("/usr/include/c++/4.2.1" "std c++")
                                     ("/Data/P4/" "P4")
                                     ("/Users/frinkr/Desktop/Dropbox/Tech/" "Tech"))))

       (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
       (setq ecb-use-speedbar-instead-native-tree-buffer nil)

       (defun ecb-toggle-windows ()
         "toggle ecb-activaty"
         (interactive)
         (ecb-toggle-ecb-windows)
         )

       (global-set-key (kbd "M-SPC") 'ecb-toggle-windows)

       )
     )
(fx/setup-ecb)

;;;;
;;;;                        python
;;;;
(defun fx/setup-python()
  ;; auto env
  (when nil
    (require 'auto-virtualenv)
    (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
    )

  ;; python shell
  (setq gud-pdb-command-name "python -m pdb ")
  (defalias 'python 'run-python)

  (elpy-enable)

  (when (require 'flycheck nil t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (require 'py-autopep8)
  (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
  )


;;;;
;;;;                        yasnippets
;;;;
(defun fx/setup-yasnippets()
  (require 'yasnippet)
  (yas-global-mode t)

  ;; Remove Yasnippet's default tab key binding
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  ;; Set Yasnippet's key binding to shift+tab
  (define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand)
  ;; Alternatively use Control-c + tab
  (define-key yas-minor-mode-map (kbd "\C-c TAB") 'yas-expand)
  )

;;;;
;;;;           helm
;;;;
(defun fx/setup-helm()
  (require 'helm-config)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  (global-set-key (kbd "C-x m") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )

(when t
  (global-set-key (kbd "C-S-l") 'helm-semantic-or-imenu)
  )



;;;;
;;;;           Flycheck
;;;;
(defun fx/setup-flycheck()
  (global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda () (flyspell-prog-mode)))

  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

  ;;(add-hook 'text-mode-hook 'flyspell-mode)
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (eval-after-load 'flycheck '(require 'flycheck-ghcmod))
  )

(fx/setup-flycheck)

;;;;
;;;;           cmake-ide
;;;;
(when nil
  (require 'rtags)
  (cmake-ide-setup)
  (define-key c++-mode-map [C-down-mouse-1] 'rtags-find-symbol-at-point)
  )


;;;;
;;;;               Cedet
;;;;
(defun fx/setup-cedet()
  ;; select which submodes we want to activate
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  ;;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  ;;(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
  ;;(add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode) ;; fronzen?

  ;; Activate semantic
  (semantic-mode 1)

  ;; EDE
  (global-ede-mode 1)
  (ede-enable-generic-projects)

  ;; GNU global
  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)

  ;; No stick func mode, for case of overlapping with tabbar
  ;; but replace with 'which-func-mode'
  (when (display-graphic-p)
    (global-semantic-stickyfunc-mode -1)
    (which-function-mode 1))

  ;; highlight the editing
  (global-semantic-highlight-edits-mode 1)

  ;; ac source
  (defun fx/ac-cedet-hook ()
    (add-to-list 'ac-sources 'ac-source-gtags)
    (add-to-list 'ac-sources 'ac-source-semantic))
  (add-hook 'c-mode-common-hook 'fx/ac-cedet-hook)

  ;; Keybinding
  (defun fx/cedet-hook ()
    (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
    (local-set-key [(control ?/)] 'semantic-ia-complete-symbol-menu)
    (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
    (local-set-key "\C-c=" 'semantic-decoration-include-visit)
    (local-set-key "\C-cj" 'semantic-ia-fast-jump)
    (local-set-key "\C-cq" 'semantic-ia-show-doc)
    (local-set-key "\C-cs" 'semantic-ia-show-summary)
    (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
    )
  (add-hook 'c-mode-common-hook 'fx/cedet-hook)
  (add-hook 'lisp-mode-hook 'fx/cedet-hook)
  (add-hook 'scheme-mode-hook 'fx/cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'fx/cedet-hook)
  (add-hook 'erlang-mode-hook 'fx/cedet-hook)

  ;; Appearance
  (if (display-graphic-p)
      (progn

        )
    (progn
      (set-face-attribute 'semantic-highlight-func-current-tag-face  nil :background "color-234")
      ))
  )

;;;; (fx/setup-cedet)

;;;;
;;;;           gdb many window
;;;;
(when is-macos
  (load-library "gud.el"))


;;;;
;;;;           cscope
;;;;
(defun fx/setup-cscope()
  (require 'xcscope)
  (setq cscope-do-not-update-database nil)
  ;; hotkey for cscope
  (when t
    (define-key global-map [(meta f2)]  'cscope-find-this-symbol)
    (define-key global-map [(control f6)]  'cscope-find-global-definition)
    (define-key global-map [(control f7)]  'cscope-find-global-definition-no-prompting)
    (define-key global-map [(control f8)]  'cscope-pop-mark)
    (define-key global-map [(control f9)]  'cscope-next-symbol)
    (define-key global-map [(control f10)] 'cscope-find-this-file);;cscope-next-file)
    (define-key global-map [(control f11)] 'cscope-find-this-symbol);;cscope-prev-symbol)
    (define-key global-map [(control f12)] 'cscope-find-global-definition-no-prompting);;cscope-prev-file)
    (define-key global-map [(meta f9)]  'cscope-display-buffer)
    (define-key global-map [(meta f10)] 'cscope-display-buffer-toggle)
    )
  (global-set-key (kbd "M-s d") 'cscope-find-global-definition)
  (global-set-key (kbd "M-s s") 'cscope-find-this-symbol)
  (global-set-key (kbd "M-s M-s") 'cscope-find-this-symbol)
  (global-set-key (kbd "C-S-s") 'cscope-find-this-symbol)
  (global-set-key (kbd "C-S-j") 'cscope-find-global-definition)
  (global-set-key (kbd "M-s f") 'cscope-find-this-file)
  (global-set-key (kbd "M-s t") 'cscope-find-this-text-string)
  (global-set-key (kbd "M-s u") 'sr-speedbar-select-window)
  (global-set-key (kbd "M-s k") 'sr-speedbar-toggle)
  (global-set-key (kbd "<M-down-mouse-1>") 'cscope-find-this-symbol)
  )

(fx/setup-cscope)

;; Setup Common Lisp mode
(condition-case err
    (require 'cl)
  (error (message "Unable to load Common Lisp package.")))


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
  ;;(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

  ;; Create my own coding style
  ;; No space before { and function sig indents 4 if argument overflow
  (setq fx/c4-style
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

  (setq fx/c2-style
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
  (add-hook 'c-mode-common-hook 'jk/c-mode-common-hook)


  ;; Construct a hook to be called when entering C mode
  (defun fx/c-mode ()
    (progn (define-key c-mode-base-map "\C-l" 'newline-and-indent)
           (c-add-style "fx2" fx/c2-style t)
           (c-add-style "fx4" fx/c4-style t))
    )
  (add-hook 'c-mode-common-hook 'fx/c-mode)

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
  (defun fx/asm-mode-hook ()
    (progn (setq comment-column 36)
           (setq tab-stop-list '(4 8 12 16 20 24 28 36 40 44 48))))
  (add-hook 'asm-mode-hook 'fx/asm-mode-hook)
  (add-to-list 'auto-mode-alist '("\\.s$" . asm-mode))
  (add-to-list 'auto-mode-alist '("\\.asm$" . asm-mode))

  (autoload 'cpp-font-lock "cpp-mode" "CPP Font Lock mode" t)
  )

(fx/setup-c++)

;;;;---------------------------------------------------------------------------
;;;;                     More variable settings
;;;;---------------------------------------------------------------------------

(setq custom-file "~/.emacs.d/package-selected-packages.el")
(when (file-exists-p custom-file)
  (load custom-file))

;; Add final message so using C-h l I can see if .emacs failed
(message ".emacs loaded successfully.")
