;;; packages.el --- efx layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Yuqing Jiang <frinkr@outlook.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defconst efx-packages
  '(
    highlight-symbol
    p4
    recentf
    reveal-in-osx-finder
    undo-tree
    ))

(defun efx/init-highlight-symbol()
  (use-package highlight-symbol
    :config (efx/setup-highlight-thing)
    )
  )
(defun efx/post-init-p4()
  (setenv "P4CONFIG" "p4.config"))

(defun efx/post-init-recentf()
  (setq recentf-max-menu-items 2500)
  )

(defun efx/init-reveal-in-osx-finder()
  (use-package reveal-in-osx-finder
    :defer t
    :config (defalias 'open-in-finder 'reveal-in-osx-finder)
    )
  )

(defun efx/init-undo-tree()
  (use-package undo-tree
    :config (efx/setup-undo)))

;;; packages.el ends here

