;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          general
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; line & column number
(global-hl-line-mode nil)
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; mouse in terminal
(xterm-mouse-mode t)
(tool-bar-mode -1)

;; Setup save options (auto and backup) -- still buggy need new Replace func
;; Disable backup and autosave
(setq backup-inhibited t)
(setq auto-save-default nil)
(desktop-save-mode 1)  ;; save session
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          key binding
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Set control & meta key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; set of glocal shortcut
(global-set-key "\C-xw" 'what-line)
(global-set-key "\C-z" 'undo)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\C-o" 'ff-find-other-file)
(global-set-key "\C-q" 'save-buffers-kill-terminal)
(global-set-key [(meta m)] 'set-mark-command)
(global-set-key [delete] 'delete-char)
(global-set-key [backspace] 'delete-backward-char)
(define-key isearch-mode-map [backspace] 'isearch-delete-char)
(global-set-key "\M-SPC" 'set-mark-command)
(global-set-key "\M-n" (lambda () (interactive) (next-line 5)))
(global-set-key "\M-p" (lambda () (interactive) (previous-line 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          convenient functions
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;
;;;;          new scratch
;;;;
(defun new-scratch ()
  "open up a guaranteed new scratch buffer"
  (interactive)
  (switch-to-buffer (loop for num from 0
                          for name = (format "*scratch-%03i*" num)
                          while (get-buffer name)
                          finally return name)))

(global-set-key (kbd "C-t") 'new-scratch)


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
;;;;           reload the buffer
;;;;
(defun reload ()
  "Reload the buffer w/o prompt."
  (interactive)
  (revert-buffer nil t))

(global-set-key (kbd "C-x r") 'reload)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          melpa
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun install-extra-packages()
  (require 'package)
  (add-to-list 'package-archives
               '("melpa-stable" . "https://stable.melpa.org/packages/"))
  (package-initialize)

  (setq package-list '(
                       dracula-theme))

  ;; refresh
  (unless package-archive-contents
    (package-refresh-contents))

  ;; install 
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

(install-extra-packages)

(if window-system
    (load-theme 'dracula t)
  )
