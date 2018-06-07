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

;; mac: set control & meta key
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta nil)
(setq mac-command-modifier 'control)
(setq mac-option-modifier 'meta)

;; dis evil
(define-key evil-emacs-state-map (kbd "C-z") nil)
(global-set-key (kbd "C-z") 'undo-tree-undo)
