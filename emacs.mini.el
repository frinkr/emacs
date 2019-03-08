;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;;          general
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-hl-line-mode nil)
(desktop-save-mode 1)  ;; save session

;; line & column number
(line-number-mode t)
(column-number-mode t)

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
