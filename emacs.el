;;;;               
;;;;                Global configs
;;;;
(setq x_default_root "/Users/frinkr/Desktop")
(setq x_lisp_root "/Users/frinkr/.emacs.d/lisp")
(setq x_proj_root "/Data/P4/daji_dev_mac_dp121")

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

(when nil  ;; install the required packages automatically
  (defvar frinkr/packages '(
                            auto-complete
                            cygwin-mount
                            cmake-font-lock
                            cmake-mode
                            dash
                            dired+
                            dired-details
                            dired-details+
                            dired-hacks-utils
                            dired-k
                            dired-rainbow
                            dired-single
                            dos
                            dtrace-script-mode
                            ecb
                            egg
                            fill-column-indicator
                            ghc
                            ghci-completion
                            haskell-mode
                            hlinum
                            mouse3
                            magit
                            p4
                            phi-rectangle
                            popup
                            pos-tip
                            tabbar
                            tabbar-ruler
                            )
    "Default packages")

  (defun frinkr/packages-installed-p ()
    (cl-loop for pkg in frinkr/packages
             when (not (package-installed-p pkg)) do (return nil)
             finally (return t)))

  (unless (frinkr/packages-installed-p)
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (dolist (pkg frinkr/packages)
      (when (not (package-installed-p pkg))
        (package-install pkg))))
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (eq system-type 'windows-nt)
  (add-to-list 'load-path "C:/emacs-23.3/lisp/emacs-lisp")
)

;; some package (gud.el) managed manually
(add-to-list 'load-path x_lisp_root)
(setq default-directory x_default_root)



;;;;---------------------------------------------------------------------------
;;;;                     General Settings
;;;;---------------------------------------------------------------------------


;;;;    
;;;;           Appearance Settings
;;;;
(global-hl-line-mode 1)
(setq default-cursor-type 'box)
(set-face-background hl-line-face "gray13")
(set-cursor-color "red")
(set-face-foreground 'minibuffer-prompt "yellow")


(when (not (display-graphic-p))
  (menu-bar-mode -1)
  (set-face-background 'hl-line "color-235")
  (set-face-attribute 'region nil :background "color-19")
  )

;; transparent
(set-frame-parameter (selected-frame) 'alpha '(92 85))
(add-to-list 'default-frame-alist '(alpha 92 85))


;; line & column number 
(line-number-mode t)
(column-number-mode t)
;; mouse in terminal
(xterm-mouse-mode t)   
(tool-bar-mode nil)

;; Font stuff for emacs versions that support it
;;  (this stuff seems to be the least portable, comment this stuff out if it
;;   prevents the config file from loading)
(when (eq system-type 'darwin)
  (set-face-attribute 'default nil :font "Courier New 14")   
  (add-to-list 'default-frame-alist '(font . "Courier New 15"))
  ;;(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono 16"))
  )

(when (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(font . "Consolas 11"))
  (set-face-attribute 'default nil :font "Consolas 11"))


;; Color scheme is set in customize section at bottom (dark or light)
(defconst foreground-color "white")
(defconst background-color "black")
(defconst pointer-color "red")
(defconst cursor-color "red3")

(if (featurep 'xemacs)
    (let ((frame (selected-frame)))
      (set-face-foreground 'default foreground-color)
      (set-face-background 'default background-color))
  (progn 
    (add-to-list 'default-frame-alist 
		 (cons 'foreground-color foreground-color))
    (add-to-list 'default-frame-alist 
		 (cons 'background-color background-color))
    (add-to-list 'default-frame-alist (cons 'cursor-color cursor-color))
    (set-cursor-color cursor-color)
    (set-mouse-color pointer-color)
    (if window-system
        (tool-bar-mode 0))) 
)

;; XEmacs specific setup
(if (featurep 'xemacs) 
    (progn (set-specifier default-toolbar-visible-p nil)
           (setq font-lock-use-default-colors nil)
           (setq font-lock-use-fonts t)
           (setq font-lock-use-colors t)
           (setq font-lock-maximum-decoration t)))


;; GNU specific general setup
(if (not (featurep 'xemacs))
  (condition-case err
      (progn (set-scroll-bar-mode 'right)
             (global-font-lock-mode t))
    (error (message "Not running GNU emacs 20.4 or above."))))


;;;;
;;;;        Behavior Settings
;;;;

;; Setup save options (auto and backup) -- still buggy need new Replace func
;; Disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)

;; Printing setup
(setq ps-n-up-printing 2)
(setq ps-print-header nil)

;; recent files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 2500)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(setq inhibit-splash-screen t)
(setq transient-mark-mode t)
(setq-default indent-tabs-mode nil)
(setq delete-key-deletes-forward t)
(setq mouse-yank-at-point nil)
(setq bookmark-save-flag 1)  ;; autosave bookmarks

;; upcase region is anoying
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Setup time mode
(autoload 'display-time "time" "Display Time" t)
(condition-case err
    (display-time)
  (error (message "Unable to load Time package.")))
(setq display-time-24hr-format nil)
(setq display-time-day-and-date t)
(setq display-time-mode t)

;; Setup text mode
(add-hook 'text-mode-hook '(lambda() (auto-fill-mode 1)))
(add-hook 'text-mode-hook '(lambda() (setq fill-column 80)))

;; Tab stop
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(setq tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120)))


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
  (if window-system
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
;;(global-set-key (kbd "M-p") 'scroll-down-command)
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))

(when (eq system-type 'windows-nt)
  (define-key global-map [f1] 'help-command)
  (define-key global-map [f2] 'undo)
  (define-key global-map [f3] 'isearch-forward)
  (define-key global-map [f4] 'other-window)
  (define-key global-map [f12] 'revert-buffer)
  (define-key global-map [button4] 'previous-line)
  (define-key global-map [button5] 'next-line)
  )

;;;;
;;;;           kill all other buffers but current one
;;;;
(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not '(lambda (x) (or (buffer-file-name x)
                                                (eq 'dired-mode (buffer-local-value 'major-mode x))))
                               (buffer-list)))))

(global-set-key (kbd "C-x j") 'kill-other-buffers)

;;;;
;;;;           toggle shell buffer
;;;;
(defun toggle-shell-buffer (name)
  "toggle *shell*/*eshell* buffer"
  (interactive)
  (if (string-equal name (buffer-name))
      (previous-buffer)
    (if (get-buffer name)
        (switch-to-buffer name)
      (message (format "buffer %s not exists!" name))
      )))

(if (eq system-type 'windows-nt)
    (define-key global-map "\M-`" (lambda() (interactive) (toggle-shell-buffer "*shell*")))
    (define-key global-map "\M-`" (lambda() (interactive) (toggle-shell-buffer "*eshell*"))))

;;;;
;;;;           clear *shell* buffer
;;;;
(defun clear-shell ()
  (interactive)
  (let ((comint-buffer-maximum-size 0))
    (comint-truncate-buffer)))

(defun clear-shell-hook ()
  (local-set-key "\C-l" 'clear-shell))

(add-hook 'shell-mode-hook 'clear-shell-hook)

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
  "Reload the buffer"
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
(when (equal system-type 'darwin) (set-exec-path-from-shell-PATH))


;;;;
;;;;           emacs prompt for eshell
;;;;
;;(if (eq system-type 'darwin)
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
;;;;           yascroll
;;;;
(when t
;;  (require 'yascroll)
;;  (global-yascroll-bar-mode 1)
  )


;;;;
;;;;           fill column indicator
;;;;
(when t
  (require 'fill-column-indicator)
  (setq fci-rule-width 1)
  (if (display-graphic-p)
      (setq fci-rule-color "darkblue")
    (setq fci-rule-color "green"))

  (add-hook 'after-change-major-mode-hook 'fci-mode))


;;;;
;;;;           line number
;;;;
(when t
  (require 'linum)
  (add-hook 'find-file-hook (lambda () (linum-mode 1)))
  (linum-mode t))

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
  (setq magit-last-seen-setup-instructions "1.4.0")
  (setq magit-auto-revert-mode nil)
  )


;;;;
;;;;           tabbar
;;;;
(when nil
  (setq tabbar-ruler-global-tabbar t) ; If you want tabbar
  (setq tabbar-ruler-global-ruler t) ; if you want a global ruler
  (setq tabbar-ruler-popup-menu t) ; If you want a popup menu.
  (setq tabbar-ruler-popup-toolbar t) ; If you want a popup toolbar
  (setq tabbar-ruler-popup-scrollbar t) ; If you want to only show the
                                        ; scroll bar when your mouse is moving.
  (require 'tabbar-ruler)
  )
(when t
  (require 'tabbar)
  (tabbar-mode 1)
  
  (when (not (display-graphic-p))
    (global-set-key (kbd "M-,") 'tabbar-backward)
    (global-set-key (kbd "M-.") 'tabbar-forward))
  (global-set-key (kbd "C-{") 'tabbar-backward)
  (global-set-key (kbd "C-}") 'tabbar-forward)
  ;;(global-set-key (kbd "C-M-[") 'tabbar-backward)
  ;;(global-set-key (kbd "C-M-]") 'tabbar-forward)
  (setq tabbar-buffer-groups-function
        (lambda ()
          (list "All Buffers")))

  ;; Remove buffer names from list
  (setq *tabbar-ignore-buffers* '("*CEDET Global*" "*Compile-Log*" "*Messages*" "*etags tmp*" ))
  (setq tabbar-buffer-list-function
        (lambda ()
          (remove-if
           (lambda (buffer)
             (and (not (eq (current-buffer) buffer)) ; Always include the current buffer.
                  (loop for name in *tabbar-ignore-buffers* ;remove buffer name in this list.
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


  ;; font & face
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


  ;; face
  (custom-set-faces
   '(diredp-dir-priv ((t (:foreground "cyan" :weight bold))))
   '(diredp-file-name ((t (:foreground "green"))))
   '(diredp-no-priv ((t (:background "black"))))
   '(diredp-number ((t (:foreground "yellow"))))
   )
  (when (not (display-graphic-p))
    (custom-set-faces
     '(diredp-dir-priv ((t (:foreground "cyan" :weight bold))))
     '(diredp-exec-priv ((t (:background "Color-234" :foreground "brightred"))))
     '(diredp-file-name ((t (:foreground "green"))))
     '(diredp-no-priv ((t (:background "black"))))
     '(diredp-number ((t (:foreground "yellow"))))
     '(diredp-read-priv ((t (:background "Color-234" :foreground "color-34"))))
     '(diredp-write-priv ((t (:background "color-234")))))
    )
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
    (add-to-list 'load-path (concat x_lisp_root "elscreen-1.4.6"))
    (add-to-list 'load-path (concat x_lisp_root "apel-10.8"))
    ;;(load "elscreen" "ElScreen" t)
    (global-set-key (kbd "C-. C-c") 'elscreen-create)
    (global-set-key (kbd "C-. C-k") 'elscreen-kill-screen-and-buffers)
    (global-set-key (kbd "C-. C-p") 'elscreen-previous)
    (global-set-key (kbd "C-. C-n") 'elscreen-next))
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
  (require 'haskell-mode)
  (add-to-list 'Info-default-directory-list (concat x_lisp_root "haskell-mode"))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation))


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
  (when (eq system-type 'windows-nt)
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
  (autoload 'dos-mode "dos" "Edit Dos scripts." t)
  (add-to-list 'auto-mode-alist '("\\.bat$" . dos-mode)))

;;;;
;;;;           Speed Bar
;;;;
(when nil
  (when (display-graphic-p)
    (load-file (concat x_lisp_root "sr-speedbar.el"))
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
(when t
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

  ;; menu font
  (set-face-font 'ac-candidate-face "Courier New 13")
  (set-face-font 'ac-selection-face "Courier New 13")

  (if (display-graphic-p)
      (global-set-key [(control ?/)] 'auto-complete)
    (ac-set-trigger-key "TAB"))

  ;; matching
  (setq ac-use-fuzzy t)
  (setq ac-ignore-case t)

  (setq popup-use-optimized-column-computation nil)
  (ac-linum-workaround)
  )

;;;;
;;;;           ecb
;;;;
(when (eq system-type 'darwin)
  ;; error when byte-compiling from melpa, nerver mind. It can just run
  (require 'ecb)
  (setq ecb-options-version "2.40")
  
  ;; activate at start up
  (setq ecb-auto-activate t)

  ;; no tip-of-day
  (setq ecb-tip-of-the-day nil)

  ;; layout
  (setq ecb-layout-name "left11")  
  (setq ecb-layout-window-sizes nil)
  (setq ecb-fix-window-size (quote width)) ;; fixed witdh

  ;; directories
  (setq ecb-source-path (quote (("/usr/include" "c")
                                ("/usr/include/c++/4.2.1" "std c++")
                                ("/Data/P4/" "P4")
                                ("/Users/frinkr/Desktop/Dropbox/Tech/" "Tech"))))
  
  (setq ecb-primary-secondary-mouse-buttons (quote mouse-1--C-mouse-1))
  (setq ecb-use-speedbar-instead-native-tree-buffer nil)
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
    (which-func-mode 1))

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
(when (eq system-type 'darwin)
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
  (global-set-key (kbd "<C-down-mouse-1>") 'cscope-find-global-definition-no-prompting)
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
(add-to-list 'auto-mode-alist '("\\.m\\'" . objc-mode))

;; Create my own coding style
;; No space before { and function sig indents 4 if argument overflow
(setq bws-c-style
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
         (define-key c-mode-base-map "\C-z" 'undo)
         (define-key c-mode-base-map [delete] 'c-hungry-delete-forward)
         (define-key c-mode-base-map [backspace] 'c-hungry-delete-backwards)
         (define-key c-mode-base-map [f4] 'speedbar-get-focus)
         (define-key c-mode-base-map [f5] 'next-error)
         (define-key c-mode-base-map [f6] 'run-program)
         (define-key c-mode-base-map [f7] 'compile)
         (define-key c-mode-base-map [f8] 'set-mark-command)
         (define-key c-mode-base-map [f9] 'insert-breakpoint)
         (define-key c-mode-base-map [f10] 'step-over)
         (define-key c-mode-base-map [f11] 'step-into)
         (c-add-style "Brad's Coding Style" bws-c-style t)))
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


;; Setup imenu
(add-hook 'c-mode-common-hook 'imenu-add-menubar-index)


;; Setup func-menu, the function menu quicklink package (XEmacs only)
(autoload 'function-menu "func-menu" "Load the parsing package" t)
(autoload 'fume-add-menubar-entry "func-menu" "Add function menu" t)
(autoload 'fume-list-functions "func-menu" "List functions in window" t)
(autoload 'fume-prompt-function-goto "func-menu" "Goto function" t)
(setq fume-max-items 35)
(setq fume-fn-window-position 3)
(setq fume-auto-position-popup t)
(setq fume-display-in-modeline-p t)
(setq fume-menubar-menu-location "Info")
(setq fume-buffer-name "Function List*")
(setq fume-no-prompt-on-valid-default nil)
;(global-set-key [f8] 'function-menu)
;(define-key global-map "\C-cl" 'fume-list-functions)
;(define-key global-map "\C-cg" 'fume-prompt-function-goto)
(condition-case err
    (progn (function-menu)
           (add-hook 'c-mode-common-hook 'fume-add-menubar-entry))
  (error (message "Unable to load Function Menu package")))


;; Setup imenu
;(setq imenu-sort-function 'imenu--sort-by-name)
;(setq imenu-max-items 45)
;(define-key global-map "\C-cj" 'imenu)

;; Setup my own packages
;;(add-to-list 'load-path (expand-file-name "~/elisp/"))
(autoload 'cpp-font-lock "cpp-mode" "CPP Font Lock mode" t)


;;;;---------------------------------------------------------------------------
;;;;                     More variable settings
;;;;---------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(grep-command "grep -nH -i -e")
 '(semantic-idle-scheduler-idle-time 0.5))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-face-tag ((t (:foreground "brightblue"))))
 '(diredp-dir-priv ((t (:foreground "cyan" :weight bold))))
 '(diredp-exec-priv ((t (:background "Color-234" :foreground "OrangeRed3"))))
 '(diredp-file-name ((t (:foreground "green"))))
 '(diredp-no-priv ((t (:background "black"))))
 '(diredp-number ((t (:foreground "yellow"))))
 '(diredp-read-priv ((t (:background "Color-234" :foreground "white"))))
 '(diredp-write-priv ((t (:background "color-234" :foreground "cyan"))))
 '(ecb-default-highlight-face ((t (:background "#3A8440"))))
 '(ecb-history-bucket-node-face ((t (:inherit ecb-bucket-node-face :foreground "gray70"))))
 '(ecb-history-dead-buffer-face ((t (:inherit ecb-history-general-face :foreground "gray30"))))
 '(ecb-history-general-face ((t (:inherit ecb-default-general-face :height 1.0))))
 '(haskell-constructor-face ((t (:foreground "green"))))
 '(haskell-definition-face ((t (:foreground "magenta"))))
 '(link ((t (:foreground "green" :underline t))))
 '(semantic-highlight-edits-face ((t (:background "color-234"))))
 '(semantic-unmatched-syntax-face ((((class color) (background dark)) (:underline nil)))))



;; Add final message so using C-h l I can see if .emacs failed
(message ".emacs loaded successfully.")
