;;;;               
;;;;                Global configs
;;;;
(setq is-macos (eq system-type 'darwin))
(setq is-windows (eq system-type 'windows-nt))
(setq user-lisp-root (expand-file-name "~/.emacs.d/lisp"))

;;;;
;;;;           proxy
;;;;
(when t
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
  ;;(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
  ;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize) ;; You might already have this line
  )

(when t  ;; install the required packages automatically
  (setq package-list '(
                       ac-octave
                       auto-complete
                       auto-virtualenv
                       bm
                       bison-mode
                       cygwin-mount
                       cmake-font-lock
                       cmake-mode
                       clang-format
                       dash
                       dired+
                       dired-details
                       dired-details+
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
                       highlight-thing
                       highlight-symbol
                       lua-mode
                       mouse3
                       magit
                       markdown-mode
                       nyan-mode
                       neotree
                       p4
                       phi-rectangle
                       py-autopep8
                       popup
                       pos-tip
                       icicles
                       ido-vertical-mode
                       on-screen
                       osx-dictionary
                       reveal-in-osx-finder
                       smooth-scroll
                       smooth-scrolling
                       tabbar
                       tabbar-ruler
                       xcscope
                       yascroll
                       yaml-mode
                       ;; top themes
                       zenburn-theme
                       solarized-theme
                       alect-themes
                       monokai-theme
                       moe-theme
                       material-theme
                       )
        )


  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when is-windows
  (add-to-list 'load-path "C:/emacs-23.3/lisp/emacs-lisp")
)

;; some package (gud.el) managed manually
(add-to-list 'load-path user-lisp-root)



;;;;---------------------------------------------------------------------------
;;;;                     General Settings
;;;;---------------------------------------------------------------------------


;;;;
;;;;        Behavior Settings
;;;;

(global-hl-line-mode 1)
(desktop-save-mode 1)  ;; save session

;; transparent
(set-frame-parameter (selected-frame) 'alpha '(95 90))
(add-to-list 'default-frame-alist '(alpha 95 90))

;; dark theme

(setq dark-theme t)

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

;; Auto close bracket insertion
(electric-pair-mode 1)

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
(defun set-frame-size-according-to-resolution ()
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
  (set-frame-size-according-to-resolution))


;;;;
;;;;           Set control & meta key
;;;;
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)


;;;;
;;;;           set of glocal shortcut
;;;;
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
)

;;;;
;;;;          ido
;;;;
(when t
  (require 'ido)
  (require 'ido-vertical-mode)

  (setq ido-everywhere t)
  
  (ido-mode t)
  (ido-vertical-mode 1)
  )

;;;;
;;;;          ibuffer
;;;;
(when t
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (autoload 'ibuffer "ibuffer" "List buffers." t)
  )

;;;;
;;;;          scrolling
;;;;
(when t
  (require 'on-screen)
  (on-screen-global-mode +1)

;;  (require 'smooth-scroll)
;;  (smooth-scroll-mode t)

  (require 'smooth-scrolling)
  (when t
    (setq mouse-wheel-progressive-speed nil) ;; make the scrolling slower
    (setq mouse-wheel-scroll-amount '(3 ((shift) . 3))) ;; 3 lines at a time
    (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
    (setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
    (setq scroll-step 3) ;; keyboard scroll 3 lines at a time
    )

  
  ;; yascroll
  (when nil
    (require 'yascroll)
    (global-yascroll-bar-mode 1)
    (setq yascroll:delay-to-hide nil)
    
    (scroll-bar-mode -1)
    )
  )


;;;;
;;;;           bookmark
;;;;
(when t
  (global-set-key (kbd "<left-margin> <mouse-1>") 'bm-toggle)
  (require 'bm)
  )


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
;;;;           toggle shell buffer
;;;;
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
;;;;
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
;;;;           drag on Mac OSX
;;;;

(global-set-key [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)
(when nil
  (define-key global-map [ns-drag-file] 'my-ns-open-files)
  (defun my-ns-open-files ()
    "Open files in the list `ns-input-file'."
    (interactive)
    (mapc 'find-file ns-input-file)
    (setq ns-input-file nil))
  )


;;;;
;;;;          reveal in Finder  
;;;;
(when t
  (require 'reveal-in-osx-finder)
  (defalias 'open-in-finder 'reveal-in-osx-finder)
 )


;;;;
;;;;           clear *shell* buffer
;;;;
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

;;;;
;;;;           kill *Completions* buffer automatically
;;;;
(defun delete-completion-window-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (delete-window win)
      (kill-buffer "*Completions*")))
  output)

(add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer)


;;;;
;;;;           reload the buffer
;;;;
(defun reload ()
  "Reload the buffer."
  (interactive)
  (revert-buffer))

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
(defun find-alternative-file-with-sudo ()
  "Open current buffer as root!"
  (interactive)
  (when buffer-file-name
    (find-alternate-file
     (concat "/sudo::"
             buffer-file-name))))

(global-set-key (kbd "C-x C-k") 'find-alternative-file-with-sudo)


;;;;             
;;;;           set emacs PATH
;;;;
(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell 
      (replace-regexp-in-string "[[:space:]\n]*$" "" 
        (shell-command-to-string "$SHELL -l -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(when is-macos
  (set-exec-path-from-shell-PATH))


;;;;
;;;;           emacs prompt for eshell
;;;;
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


;;;; 
;;;;           mouse3
;;;;
(when t
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
(when t
  (require 'highlight-symbol)
  (global-set-key [double-mouse-1] 'highlight-symbol)
;;  (global-set-key [mouse-1] 'highlight-symbol-remove-all)

  (if dark-theme
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
    (setq highlight-symbol-colors
          '(
            ("black" . "SpringGreen3")
            ("white" . "DarkOrange")
          ))
    )

  (defun highlight-symbol-next-color-pair ()
    "Step to and return next color from the color ring."
    (let ((color (nth highlight-symbol-color-index
                      highlight-symbol-color-pairs)))
      (if color ;; wrap
          (incf highlight-symbol-color-index)
        (setq highlight-symbol-color-index 1
              color (car highlight-symbol-color-pairs)))
      color))

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
        )))

  ;;  (require 'highlight-thing)
;;  (global-highlight-thing-mode)
;;  (setq highlight-thing-delay-seconds 0.2)
  )

;;;;
;;;;           helm
;;;;
(when nil
  (require 'helm-config)
  (global-set-key (kbd "C-x b") 'helm-buffers-list)

  (global-set-key (kbd "C-x m") 'helm-M-x)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )

;;;;
;;;;           fill column indicator
;;;;
(when t
  (require 'fill-column-indicator)
  (setq fci-rule-width 1)
  (setq fci-rule-column 90)
  (if (display-graphic-p)
      (setq fci-rule-color "darkblue")
    (setq fci-rule-color "green"))

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


;;;;
;;;;           line number
;;;;
(when t
  (require 'linum)
  (add-hook 'find-file-hook (lambda () (linum-mode 1)))

  ;; better appearance for terminal
  (when (not (display-graphic-p))
    (defun linum-format-func (line)
      (let ((w (length (number-to-string (count-lines (point-min) (point-max))))))
        (propertize (format (format "%%%dd| " w) line) 'face 'linum)))
    (setq linum-format 'linum-format-func)
    )

  (global-linum-mode 1)
  (linum-mode t)
  )

;;;;
;;;;           P4
;;;;
(when t
  (load-library "p4")
  (setenv "P4CONFIG" "p4.config"))


;;;;
;;;;           magit
;;;;
(when t
  (require 'magit)

  (setq magit-auto-revert-mode nil)
  
  (global-set-key (kbd "C-x g") 'magit-status)
  (defalias 'magit 'magit-status)
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
  (defun ac-octave-mode-setup ()
    (setq ac-sources '(ac-source-octave)))
  (add-hook 'octave-mode-hook
            '(lambda () (ac-octave-mode-setup)))

  (add-hook 'inferior-octave-mode-hook
            (lambda ()
              (turn-on-font-lock)
              (define-key inferior-octave-mode-map [up]
                'comint-previous-input)
              (define-key inferior-octave-mode-map [down]
                'comint-next-input)))
  )

;;;;
;;;;           tabbar
;;;;
(when t
  (require 'tabbar)
  (tabbar-mode 1)
  
  (when (not (display-graphic-p))
    (global-set-key (kbd "M-,") 'tabbar-backward)
    (global-set-key (kbd "M-.") 'tabbar-forward))
  (global-set-key (kbd "C-{") 'tabbar-backward)
  (global-set-key (kbd "C-}") 'tabbar-forward)
  (global-set-key (kbd "C-t") 'new-scratch)
  ;;(global-set-key (kbd "C-M-[") 'tabbar-backward)
  ;;(global-set-key (kbd "C-M-]") 'tabbar-forward)
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
              (concat " > " (concat ad-return-value " "))
            (concat " " (concat ad-return-value " ")))))
  ;; Called each time the modification state of the buffer changed.
  (defun ztl-modification-state-change ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))
  ;; First-change-hook is called BEFORE the change is made.
  (defun ztl-on-buffer-modification ()
    (set-buffer-modified-p t)
    (ztl-modification-state-change))
  (add-hook 'after-save-hook 'ztl-modification-state-change)
  ;; This doesn't work for revert, I don't know.
  ;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
  (add-hook 'first-change-hook 'ztl-on-buffer-modification)

  (setq tabbar-use-images nil)
  )


;;;;
;;;;           Dired
;;;;
(when t
  ;; show details by default
  (setq diredp-hide-details-initially-flag nil)
  (require 'dired+)
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

  (add-hook 'dired-mode-hook 'frinkr/dired-mode-hook)
  (defun frinkr/dired-mode-hook ()
    (local-set-key (kbd "<mouse-2>") 'diredp-mouse-find-file-reuse-dir-buffer))
  
  )


;;;;
;;;;            Mouse Scroll in terminal
;;;;
(when (not (display-graphic-p))

  ;; Mousewheel
  (defun sd-mousewheel-scroll-up (event)
    "Scroll window under mouse up by five lines."
    (interactive "e")
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn 
            (select-window (posn-window (event-start event)))
            (scroll-up 5))
        (select-window current-window))))

  (defun sd-mousewheel-scroll-down (event)
    "Scroll window under mouse down by five lines."
    (interactive "e")
    (let ((current-window (selected-window)))
      (unwind-protect
          (progn 
            (select-window (posn-window (event-start event)))
            (scroll-down 5))
        (select-window current-window))))

  (global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
  (global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)
  )


;;;;
;;;;           fast-nav mode
(when t
  (defvar fast-nav-mode-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-n") (lambda () (interactive) (next-line 5)))
      (define-key map (kbd "M-p") (lambda () (interactive) (previous-line 5)))
      (define-key map (kbd "M-b") (lambda () (interactive) (backward-word)))
      
      (global-set-key (kbd "C-{") 'tabbar-backward)
      (global-set-key (kbd "C-}") 'tabbar-forward)
      (global-set-key (kbd "C-t") 'new-scratch)

      (define-key global-map [(meta m)] 'set-mark-command)
      (global-set-key (kbd "<mouse-2>") (lambda () (interactive) (kill-buffer (current-buffer))))
      (global-set-key (kbd "<mouse-3>") 'tabbar-forward)
      (global-set-key (kbd "<mouse-5>") 'tabbar-backward)

      map)
    "fast-nav-mode keymap.")

  (define-minor-mode fast-nav-mode
    "A minor mode so that my key settings override annoying major modes."
    :init-value t
    :lighter " fast-nav"
    :keymap fast-nav-mode-map)

  (defun fast-nav/minibuffer-setup-hook ()
    (fast-nav-mode 0))
  (add-hook 'minibuffer-setup-hook 'fast-nav/minibuffer-setup-hook)
  
  (fast-nav-mode 1)
  
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
  
  (diminish 'fast-nav-mode nil)
  )



;;;;
;;;;           rectangular select
;;;;
(when t
  (require 'phi-rectangle)
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
;;;;           yacc and lex
;;;;
(when t
  (require 'bison-mode)
  (add-to-list 'auto-mode-alist '("\\.lm\\'" . bison-mode))
  (add-to-list 'auto-mode-alist '("\\.ym\\'" . bison-mode))
  )

;;;;
;;;;           ansi color
;;;;
(when t
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  )


;;;;
;;;;           elscreen
;;;;
(when nil
  (when (display-graphic-p) 
    (add-to-list 'load-path (concat user-lisp-root "elscreen-1.4.6"))
    (add-to-list 'load-path (concat user-lisp-root "apel-10.8"))
    ;;(load "elscreen" "ElScreen" t)
    (global-set-key (kbd "C-. C-c") 'elscreen-create)
    (global-set-key (kbd "C-. C-k") 'elscreen-kill-screen-and-buffers)
    (global-set-key (kbd "C-. C-p") 'elscreen-previous)
    (global-set-key (kbd "C-. C-n") 'elscreen-next))
  )


;;;;
;;;;           Flycheck
;;;;
(when t
  (global-flycheck-mode)
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))
  (add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++11")))
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)

  ;;(add-hook 'text-mode-hook 'flyspell-mode)
  ;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (eval-after-load 'flycheck '(require 'flycheck-ghcmod))
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
;;;;                        python
;;;;
(when nil
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
(when nil
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
;;;;           delete the 'complete' buffer
;;;;
(when t
  (defun delete-completion-window-buffer (&optional output)                                                                
    (interactive)                                                                                                
    (dolist (win (window-list))                                                                                  
      (when (string= (buffer-name (window-buffer win)) "*Completions*")                                          
        (delete-window win)                                                                                      
        (kill-buffer "*Completions*")))                                                                          
    output)
  (add-hook 'comint-preoutput-filter-functions 'delete-completion-window-buffer))
  
;;;;
;;;;           cygwin in emacs
;;;;
(when t
  ;; Cgywin Emacs
  (when (eq system-type 'cygwin)
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)
    )

  ;; NTEmacs
  (when is-windows
    (setenv "PATH" (concat "C:/cygwin/bin;" (getenv "PATH")))
    (setq exec-path (cons "C:/cygwin/bin/" exec-path))

    (require 'cygwin-mount)
    (cygwin-mount-activate)

    ;; Remove the unsightly chars
    (add-hook 'comint-output-filter-functions
              'shell-strip-ctrl-m nil t)
    (add-hook 'comint-output-filter-functions
              'comint-watch-for-password-prompt nil t)

    (when nil
      (setq explicit-shell-file-name "C:/cygwin/Cygwin_x86_64 vc12.bat")
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


;;;;
;;;;           Speed Bar
;;;;
(when nil
  (when (display-graphic-p)
    (load-file (concat user-lisp-root "sr-speedbar.el"))
    (require 'sr-speedbar)

    ;; Setup speedbar, an additional frame for viewing source files
    (autoload 'speedbar-frame-mode "speedbar" "Popup a speedbar frame" t)
    (autoload 'speedbar-get-focus "speedbar" "Jump to speedbar frame" t)
    (autoload 'speedbar-toggle-etags "speedbar" "Add argument to etags command" t)
    (setq speedbar-frame-plist '(minibuffer nil
                                            border-width 0
                                            internal-border-width 0
                                            menu-bar-lines 0
                                            modeline t
                                            name "SpeedBar"
                                            width 24
                                            unsplittable t))
    (condition-case err
        (progn (speedbar-toggle-etags "-C"))
      (error (message "Unable to load Speedbar package.")))

    (global-set-key [(meta f4)] 'speedbar-get-focus)))


;;;;
;;;;           C++11
;;;;
(when nil ;; depricated in emacs 25
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
(when t
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


;;;;
;;;;           ecb
;;;;
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


;;;;
;;;;                     theme
;;;;
(when t
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
        (set-face-background 'hl-line "color-235")
        (set-face-attribute 'region nil :background "color-19")        
        )
      )
    )

  (when t
    
    ;; Font stuff for emacs versions that support it
    ;;  (this stuff seems to be the least portable, comment this stuff out if it
    ;;   prevents the config file from loading)
    (when is-macos
      ;;(set-face-attribute 'default nil :font "Menlo 12")
      (set-face-attribute 'default nil :font "Anonymous Pro-14")
      (add-to-list 'default-frame-alist '(font . "Anonymous Pro-14"))
      ;; fix gap at top when maximized
      (setq frame-resize-pixelwise t)
      )

    (when is-windows
      (add-to-list 'default-frame-alist '(font . "Consolas 11"))
      (set-face-attribute 'default nil :font "Consolas 11"))
    )

  ;; tabbar face
  (when t
    (when (not (display-graphic-p))
      (set-face-attribute
       'tabbar-default nil
       :background "gray20"
       :foreground "gray40"
       :underline nil
       :box '(:line-width 1 :color "gray20" :style nil))
      (set-face-attribute
       'tabbar-unselected nil
       :background "gray20"
       :foreground "gray60"
       :box '(:line-width 1 :color "gray30" :style nil))
      (set-face-attribute
       'tabbar-selected nil
       :background "color-18"
       :foreground "brightwhite"
       :underline t
       :box '(:line-width 1 :color "gray75" :style nil))
      (set-face-attribute
       'tabbar-highlight nil
       :background "white"
       :foreground "black"
       :underline nil
       :box '(:line-width 5 :color "white" :style nil))
      (set-face-attribute
       'tabbar-button nil
       :box '(:line-width 1 :color "gray20" :style nil))
      (set-face-attribute
       'tabbar-separator nil
       :background "gray20"
       :height 0.6)

      ;; Change padding of the tabs
      ;; we also need to set separator to avoid overlapping tabs by highlighted tabs
      (custom-set-variables
       '(tabbar-separator (quote (0.5))))
      )

    (when (display-graphic-p)
      (defvar tabbar-fg              "#DCDCCC")
      (defvar tabbar-bg              "#073642")    
      (defvar tabbar-bg-selected     "#002b36")
      (defvar tabbar-bg-hightlight   "#eee8d5")
      
      (set-face-attribute
       'tabbar-default nil
       :background tabbar-bg
       :foreground tabbar-fg
       :height 2
       :underline nil
       :box nil
       )
      (set-face-attribute
       'tabbar-unselected nil
       :foreground tabbar-fg
       :background tabbar-bg
       :box nil
       )
      (set-face-attribute
       'tabbar-selected nil
       :foreground "green"
       :background tabbar-bg-selected
       :box '(:line-width -1 :style pressed-button))
      (set-face-attribute
       'tabbar-highlight nil
       :background tabbar-bg-hightlight
       :foreground "black"
       :underline nil
       :box '(:line-width 3 :color "#eee8d5" :style nil))
      (set-face-attribute
       'tabbar-button nil
       :foreground tabbar-fg
       :background tabbar-bg
       :box nil)
      (set-face-attribute
       'tabbar-separator nil
       :background tabbar-bg
       :height 0.6)    
      (custom-set-variables
       '(tabbar-separator (quote (0.2))))
      )
    )
  )


;;;;---------------------------------------------------------------------------
;;;;                     Programming Settings
;;;;---------------------------------------------------------------------------

;;;;
;;;;               Cedet
;;;;
(when t
  ;; select which submodes we want to activate
  (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
  ;;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
  (add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
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
  (defun frinkr/ac-cedet-hook ()
    (add-to-list 'ac-sources 'ac-source-gtags)
    (add-to-list 'ac-sources 'ac-source-semantic))
  (add-hook 'c-mode-common-hook 'frinkr/ac-cedet-hook)

  ;; Keybinding
  (defun frinkr/cedet-hook ()
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
  (add-hook 'c-mode-common-hook 'frinkr/cedet-hook)
  (add-hook 'lisp-mode-hook 'frinkr/cedet-hook)
  (add-hook 'scheme-mode-hook 'frinkr/cedet-hook)
  (add-hook 'emacs-lisp-mode-hook 'frinkr/cedet-hook)
  (add-hook 'erlang-mode-hook 'frinkr/cedet-hook)

  ;; Appearance
  (if (display-graphic-p)
      (progn
        
        )
    (progn
      (set-face-attribute 'semantic-highlight-func-current-tag-face  nil :background "color-234")
      ))
  )


;;;;
;;;;           gdb many window
;;;;
(when is-macos
  (load-library "gud.el"))


;;;;
;;;;           cscope
;;;;
(when t
  (require 'xcscope)
  (setq cscope-do-not-update-database t)
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
  (global-set-key (kbd "M-s f") 'cscope-find-this-file)
  (global-set-key (kbd "M-s t") 'cscope-find-this-text-string)
  (global-set-key (kbd "M-s u") 'sr-speedbar-select-window)
  (global-set-key (kbd "M-s k") 'sr-speedbar-toggle)
  (global-set-key (kbd "<M-down-mouse-1>") 'cscope-find-this-symbol)
  )

;; Setup Common Lisp mode
(condition-case err
    (require 'cl)
  (error (message "Unable to load Common Lisp package.")))


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
(setq frinkr/c4-style
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

(setq frinkr/c2-style
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

;; Construct a hook to be called when entering C mode
(defun lconfig-c-mode ()
  (progn (define-key c-mode-base-map "\C-l" 'newline-and-indent)
         (c-add-style "frinkr4" frinkr/c4-style t))
  )
(add-hook 'c-mode-common-hook 'lconfig-c-mode)


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
(defun lconfig-asm-mode-hook ()
  (progn (setq comment-column 36)
         (setq tab-stop-list '(4 8 12 16 20 24 28 36 40 44 48))))
(add-hook 'asm-mode-hook 'lconfig-asm-mode-hook)
(add-to-list 'auto-mode-alist '("\\.s$" . asm-mode))
(add-to-list 'auto-mode-alist '("\\.asm$" . asm-mode))

(autoload 'cpp-font-lock "cpp-mode" "CPP Font Lock mode" t)


;;;;---------------------------------------------------------------------------
;;;;                     More variable settings
;;;;---------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes t)
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.50")
 '(grep-command "grep -nH -i -e")
 '(haskell-interactive-mode-hide-multi-line-errors nil)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 '(package-selected-packages
   (quote
    (bison-mode rainbow-mode highlight-thing rainbow-delimiters highlight-symbol markdown-mode lua-mode ac-octave dark-souls col-highlight better-shell delight diminish osx-browse osx-dictionary zenburn-theme yascroll yafolding xkcd xcscope tabbar-ruler swiper sublimity sublime-themes sr-speedbar solarized-theme smooth-scrolling smooth-scroll py-autopep8 pos-tip phi-rectangle p4 origami on-screen nyan-prompt nyan-mode neotree mouse3 monokai-theme moe-theme minimap minesweeper material-theme ido-vertical-mode icicles hlinum helm ghci-completion ghc fold-this fold-dwim flycheck-irony flycheck-haskell flycheck-ghcmod fill-column-indicator elpy ecb dtrace-script-mode drag-stuff dockerfile-mode direx dired-single dired-rainbow dired-k dired-details+ dired+ cygwin-mount cmake-font-lock clang-format bm auto-virtualenv auto-complete alect-themes 2048-game)))
 '(semantic-idle-scheduler-idle-time 0.5)
 '(send-mail-function (quote sendmail-send-it))
 '(tabbar-separator (quote (0.2))))

;; Add final message so using C-h l I can see if .emacs failed
(message ".emacs loaded successfully.")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
